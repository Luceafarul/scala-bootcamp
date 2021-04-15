package com.evolutiongaming.bootcamp.tf.shopping.services

import cats.Monad
import cats.data.{EitherT, OptionT}
import cats.syntax.all._
import com.evolutiongaming.bootcamp.tf.shopping.domain.card.Card
import com.evolutiongaming.bootcamp.tf.shopping.domain.order.OrderId
import com.evolutiongaming.bootcamp.tf.shopping.domain.payment.Payment
import com.evolutiongaming.bootcamp.tf.shopping.domain.user.UserId

sealed trait CheckoutError

case object CartNotFound extends CheckoutError

/*
During checkout process we need to:
- find cart
- create payment
- create order
- delete cart
 */

trait CheckoutService[F[_]] {
  def checkout(userId: UserId, card: Card): F[Either[CheckoutError, OrderId]]
}

object CheckoutService {
  def apply[F[_]: Monad](
    orderService: OrderService[F],
    paymentService: PaymentService[F],
    shoppingCartService: ShoppingCartService[F]
  ): CheckoutService[F] = new CheckoutService[F] {
    override def checkout(userId: UserId, card: Card): F[Either[CheckoutError, OrderId]] = {
      val result: EitherT[F, CheckoutError, OrderId] = for {
        cart <- EitherT.fromOptionF(shoppingCartService.get(userId), CartNotFound: CheckoutError)
        payment = Payment(userId, cart.total, card)
        paymentId <- EitherT.liftF(paymentService.process(payment))
        orderId <- EitherT.liftF(orderService.create(userId, paymentId, cart.items, cart.total))
        _ <- EitherT.liftF(shoppingCartService.delete(userId))
      } yield orderId

      result.value
    }
  }
}
