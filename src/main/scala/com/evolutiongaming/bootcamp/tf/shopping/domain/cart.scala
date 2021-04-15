package com.evolutiongaming.bootcamp.tf.shopping.domain

import com.evolutiongaming.bootcamp.tf.shopping.domain.item.ItemId
import com.evolutiongaming.bootcamp.tf.shopping.domain.money.Money
import com.evolutiongaming.bootcamp.tf.shopping.domain.user.UserId
import io.circe.generic.JsonCodec
import io.circe.{Decoder, Encoder}

import java.util.Currency

object cart {

  @JsonCodec final case class Quantity(amount: Long)
  @JsonCodec final case class CartTotal(items: List[CartItem], total: Money)
  @JsonCodec final case class CartItem(itemId: ItemId, quantity: Quantity, price: BigDecimal)
  @JsonCodec final case class Cart(userId: UserId, items: List[CartItem], currency: Currency)

  implicit val currencyEncoder: Encoder[Currency] = Encoder[String].contramap(_.getCurrencyCode)
  implicit val currencyDecoder: Decoder[Currency] = Decoder[String].map(Currency.getInstance)

}
