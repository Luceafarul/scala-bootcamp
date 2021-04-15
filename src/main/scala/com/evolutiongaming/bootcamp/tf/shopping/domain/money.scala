package com.evolutiongaming.bootcamp.tf.shopping.domain

import io.circe.generic.JsonCodec

import java.util.Currency

object money {

  @JsonCodec final case class Money(amount: BigDecimal, currency: Currency)

}
