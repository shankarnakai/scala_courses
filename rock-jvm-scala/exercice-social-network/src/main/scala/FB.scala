type Name = String
type Friends = Set[Name]
type Person = (Name, Friends)
type Network = Map[Name, Friends]

object Person {
  def apply(name: Name, friends: Friends = Set.empty[Name]): Person = (name -> friends)
}

object FB extends App {
  
  def addPerson(network: Network, name: Name): Network = network + Person(name)
  
  def friend(network: Network, name: Name, friend: Name): Network = {
    val yourFriends = network.getOrElse(name, Set.empty[Name])
    val hisFriends = network.getOrElse(friend, Set.empty[Name])
    network + Person(name, yourFriends + friend) + Person(friend, hisFriends + name)
  }
  
  def unfriend(network: Network, name: Name, friend: Name): Network = {
    val yourFriends = network.getOrElse(name, Set.empty[Name])
    val hisFriends = network.getOrElse(friend, Set.empty[Name])
    network + Person(name, yourFriends - friend) + Person(friend, hisFriends - name)
  }
  
  def removePerson(network: Network, person: Name): Network =
    if(!network.contains(person)) network
    else {
      val friends = network(person)
      val newNetwork = friends.foldLeft(network){
        (networkAcc, friendName) => unfriend(networkAcc, friendName, person)
      }
      newNetwork - person
    }
  
  def getPersonWithMaxFriend(network: Network): Person = network
    .maxBy(value => value._2.size)
  
  def gePersonWithoutFriend(network: Network): List[Person] = network
    .filter(value => value._2.size == 0)
    .toList
  
  def hasSocialConnection(network: Network, person1: Name, person2: Name): Boolean =
    network(person1).contains(person2)
}


