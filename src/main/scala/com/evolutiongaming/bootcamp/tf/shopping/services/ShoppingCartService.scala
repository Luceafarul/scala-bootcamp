package com.evolutiongaming.bootcamp.tf.shopping.services


import cats.effect.Sync
import cats.syntax.all._
import cats.effect.concurrent.Ref
import com.evolutiongaming.bootcamp.tf.shopping.domain.cart._
import com.evolutiongaming.bootcamp.tf.shopping.domain.item._
import com.evolutiongaming.bootcamp.tf.shopping.domain.money.Money
import com.evolutiongaming.bootcamp.tf.shopping.domain.user._

sealed trait CartError

trait ShoppingCartService[F[_]] {
  def add(userId: UserId, itemId: ItemId, quantity: Quantity, price: Money): F[Either[CartError, Unit]]

  def get(userId: UserId): F[Option[CartTotal]]

  def delete(userId: UserId): F[Unit]
}

object ShoppingCartService {
  def apply[F[_] : Sync](): F[ShoppingCartService[F]] = {
    Ref.of(Map.empty[UserId, Cart]).map { ref =>
      new ShoppingCartService[F] {
        override def add(userId: UserId, itemId: ItemId, quantity: Quantity, price: Money): F[Either[CartError, Unit]] = {
          val cartItem = CartItem(itemId, quantity, price.amount)
          ref.modify { userCarts =>
            val userCart = userCarts.get(userId)
            val updatedCart = userCart match {
              case Some(cart) => cart.copy(items = cartItem :: cart.items)
              case None => Cart(userId, List(cartItem), price.currency)
            }
            (userCarts + (userId -> updatedCart), Right(()))
          }
        }

        override def get(userId: UserId): F[Option[CartTotal]] =
          ref.get.map { userCarts =>
            val userCart = userCarts.get(userId)
            userCart.map { cart =>
              CartTotal(cart.items, Money(cart.items.map(_.price).sum, cart.currency))
            }
          }

        override def delete(userId: UserId): F[Unit] =
          ref.update(_.removed(userId))
      }
    }
  }
}