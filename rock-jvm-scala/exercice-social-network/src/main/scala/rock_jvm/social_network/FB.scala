package rock_jvm.social_network

import scala.annotation.tailrec

object FB extends App {
  type Name = String
  type Friends = Set[Name]
  type Person = (Name, Friends)
  type Network = Map[Name, Friends]
  
  object Person {
    def apply(name: Name, friends: Friends = Set.empty[Name]): Person = (name -> friends)
  }
  
  def add(network: Network, name: Name): Network =
    if(!network.contains(name)) network + Person(name)
    else network
  
  def friend(network: Network, name: Name, friend: Name): Network = {
    val yourFriends = network.getOrElse(name, Set.empty[Name])
    val hisFriends = network.getOrElse(friend, Set.empty[Name])
    network + Person(name, yourFriends + friend) + Person(friend, hisFriends + name)
  }
  
  def unFriend(network: Network, name: Name, friend: Name): Network = {
    val yourFriends = network.getOrElse(name, Set.empty[Name])
    val hisFriends = network.getOrElse(friend, Set.empty[Name])
    network + Person(name, yourFriends - friend) + Person(friend, hisFriends - name)
  }
  
  def remove(network: Network, person: Name): Network =
    if(!network.contains(person)) network
    else {
      val friends = network(person)
      val newNetwork = friends.foldLeft(network) {
        (networkAcc, friendName) => unFriend(networkAcc, friendName, person)
      }
      newNetwork - person
    }
  
  def getPersonWithMaxFriend(network: Network): Person = network
    .maxBy(value => value._2.size)
  
  def getPersonWithoutFriend(network: Network): List[Person] = network
    .filter(value => value._2.size == 0)
    .toList
  
  def hasSocialConnection(network: Network, person1: Name, person2: Name): Boolean = {
    @tailrec
    def bfs(target: Name, considered: Friends, discovered: Friends, network: Network) : Boolean =
      if(discovered.isEmpty) false
      else {
        val name = discovered.head
        if(name == target) true
        else if (considered.contains(name)) bfs(target, considered, discovered.tail, network)
        else  bfs(target, considered + name, discovered.tail ++ network(name), network)
      }
    bfs(person1, Set.empty[Name], network(person2) + person2, network)
  }
}


