package com.evolutiongaming.bootcamp.tf.shopping.services

import cats.effect.Sync
import cats.syntax.all._
import io.circe.syntax._
import io.circe.parser.decode
import com.evolutiongaming.bootcamp.tf.shopping.domain.cart.CartItem
import com.evolutiongaming.bootcamp.tf.shopping.domain.money.Money
import com.evolutiongaming.bootcamp.tf.shopping.domain.order.{Order, OrderId}
import com.evolutiongaming.bootcamp.tf.shopping.domain.payment.PaymentId
import com.evolutiongaming.bootcamp.tf.shopping.domain.user.UserId
import com.evolutiongaming.bootcamp.tf.shopping.util.FileUtils

import java.util.UUID

trait OrderService[F[_]] {

  def create(
    userId: UserId,
    paymentId: PaymentId,
    items: List[CartItem],
    total: Money
  ): F[OrderId]

  def find(orderId: OrderId): F[Option[Order]]

}

object OrderService {

  private val fileName = "src/main/resources/orders"

  def apply[F[_]: Sync](): OrderService[F] = new OrderService[F] {
    override def create(userId: UserId, paymentId: PaymentId, items: List[CartItem], total: Money): F[OrderId] =
      for {
        id <- Sync[F].delay(UUID.randomUUID())
        orderId <- OrderId(id).pure
        order <- Order(orderId, userId, paymentId, items, total).pure
        existingOrders <- Sync[F].delay(decode[List[Order]](FileUtils.readFromFile(fileName)))
        orders <- Sync[F].fromEither(existingOrders)
        _ <- Sync[F].delay(FileUtils.writeToFile(fileName, (order :: orders).asJson.toString))
      } yield orderId

    override def find(orderId: OrderId): F[Option[Order]] =
      for {
        existingOrders <- Sync[F].delay(decode[List[Order]](FileUtils.readFromFile(fileName)))
        orders <- Sync[F].fromEither(existingOrders)
      } yield orders.find(_.id == orderId)
  }

}
