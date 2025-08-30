package demo

import com.datastax.oss.driver.api.core.CqlSession

import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.FutureConverters.CompletionStageOps

class CassandraStorage() extends Storage {

  // Connect to YugabyteDB CQL running in Docker
  val session = CqlSession.builder()
    .addContactPoint(new InetSocketAddress("127.0.0.1", 9042))
    .withLocalDatacenter("datacenter1") // Yugabyte default
    .build()

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newWorkStealingPool())

  println("✅ Connected to Cassandra")

  // Create a keyspace
  session.execute(
    """CREATE KEYSPACE IF NOT EXISTS demo
      |WITH replication = {'class': 'SimpleStrategy', 'replication_factor': 1}""".stripMargin
  )

  // Create a table
  session.execute(
    """CREATE TABLE IF NOT EXISTS demo.blocks(
      |id text PRIMARY KEY,
      |data blob
      |)""".stripMargin
  )

  // Create a table
  /*session.execute(
    """TRUNCATE TABLE demo.blocks;""".stripMargin
  )*/

  // Create a table
  session.execute(
    """CREATE TABLE IF NOT EXISTS demo.indexes (
      |id text PRIMARY KEY,
      |data blob
      |)""".stripMargin
  )

  /*
  // Create a table
  session.execute(
    """TRUNCATE TABLE demo.indexes;""".stripMargin
  )*/

  override def get(id: String): Future[Array[Byte]] = {
    session.executeAsync(session.prepare("select * from demo.blocks where id = ?").bind(id)).asScala.map { result =>
      val one = result.one()

      if(one == null) {
        throw new RuntimeException(s"Node ${id} not found!")
      }

      one.getByteBuffer("data").array()
    }
  }

  protected def saveIndex(ctx: SerializableIndexContext): Future[Boolean] = {
    session.executeAsync(session.prepare("update demo.indexes set data = ? where id = ?;")
      .bind(ByteBuffer.wrap(com.google.protobuf.any.Any.pack(ctx).toByteArray), ctx.id))
      .asScala.map(_.wasApplied())
  }

  protected def saveNode(id: String, node: Array[Byte]): Future[Boolean] = {
    session.executeAsync(session.prepare("update demo.blocks set data = ? where id = ?;")
      .bind(ByteBuffer.wrap(node), id)).asScala.map(_.wasApplied())
  }

  override def save(context: SerializableIndexContext, list: Seq[(String, Array[Byte])]): Future[Boolean] = {
    for {
      okSaveIndex <- saveIndex(context)
      okSaveBlocks <- Future.sequence(list.map{case (id, data) => saveNode(id, data)}).map(_.forall(_ == true))
    } yield {
      okSaveIndex && okSaveBlocks
    }
  }

  override def getIndex(id: String): Future[Option[SerializableIndexContext]] = {
    session.executeAsync(session.prepare("select * from demo.indexes where id = ?").bind(id)).asScala.map { result =>
      val one = result.one()

      if(one == null) {
        None
      } else {
        Some(com.google.protobuf.any.Any.parseFrom(one.getByteBuffer("data").array()).unpack(SerializableIndexContext))
      }
    }
  }

  override def close(): Unit = session.close()

  override def closeAsync(): Future[Unit] = session.closeAsync().asScala.map(_ => {})
}
