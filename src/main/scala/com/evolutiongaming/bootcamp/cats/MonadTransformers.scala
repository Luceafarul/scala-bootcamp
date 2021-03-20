package com.evolutiongaming.bootcamp.cats

import cats.Monad
import cats.data.EitherT
import cats.effect.IO
import cats.implicits._

object For {

  // API
  type Error
  type UserId
  type OrderId
  type Item

  trait Repository {

    def getFriends(userId: UserId): Either[Error, List[UserId]] = ???

    def getOrder(userId: UserId): Either[Error, OrderId] = ???

    def getItems(orderId: OrderId): Either[Error, List[Item]] = ???
  }

  class Task1(repo: Repository) {
    // find all friends for users passed
    def friends(users: List[UserId]): Either[Error, List[UserId]] =
      users.traverse(repo.getFriends).map(_.flatten)
  }

  // implement
  class Service(repo: Repository) {

    def friendsOrders(userId: UserId): Either[Error, List[Item]] =
      for {
        friends <- repo.getFriends(userId)
        orders <- friends.traverse(repo.getOrder)
        items <- orders.traverse(repo.getItems)
      } yield items.flatten

    // TODO flatMap solution, less readable then for-solution

    //      repo.getFriends(userId)
    //        .flatMap(users =>
    //          users.traverse(id => repo.getOrder(id))
    //            .flatMap(orders => orders.traverse(order => repo.getItems(order)))
    //        ).map(_.flatten)
  }

}

object MonadTransformers {

  // API
  type Error
  type UserId
  type OrderId
  type Item

  trait Repository {

    def getFriends(userId: UserId): IO[Either[Error, List[UserId]]] = ???

    def getOrder(userId: UserId): IO[Either[Error, OrderId]] = ???

    def getItems(orderId: OrderId): IO[Either[Error, List[Item]]] = ???
  }


  // implement
  class Service(repo: Repository) {

    def friendsOrders(userId: UserId): IO[Either[Error, List[Item]]] = {
      val result = for {
        friends <- EitherT(repo.getFriends(userId))
        orders <- friends.map(friend => EitherT(repo.getOrder(friend))).sequence
        items <- orders.map(order => EitherT(repo.getItems(order))).sequence
      } yield items.flatten

      result.value
    }
  }

}

object Generic {

  // API
  type Error
  type UserId
  type OrderId
  type Item

  trait Repository[F[_]] {

    def getFriends(userId: UserId): F[List[UserId]] = ???

    def getOrder(userId: UserId): F[OrderId] = ???

    def getItems(orderId: OrderId): F[List[Item]] = ???
  }

  // implement
  class Service[F[_] : Monad](repo: Repository[F]) {

    def friendsOrders(userId: UserId): F[List[Item]] = ???
  }

  //  new Repository of IO
  //  new Repository of Either
  //  new Repository of EitherT
  //  new Repository of IO Either

}
