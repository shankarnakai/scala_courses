package rock_jvm.social_network

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class FBTest extends FunSuite {
  
  test("Should add new people") {
    val network: FB.Network = Map();
    val names = List("John", "Shankar", "Jess")
    val newNetwork = names.foldLeft(network)(FB.add)
    
    newNetwork.size shouldBe names.size
    newNetwork should contain allOf(
      "John" -> Set.empty,
      "Shankar" -> Set.empty,
      "Jess" -> Set.empty
    )
  }
  
  test("Shouldn't add/overwrite when people existent in the network") {
    val person = "Shankar"
    val friend = "John"
    val network: FB.Network = FB.add(Map.empty, person)
    val addedFriendNetwork = FB.friend(network, person, friend)
    val newNetwork = FB.add(addedFriendNetwork, person)
    
    newNetwork should have size 2
    newNetwork should contain allOf(person -> Set(friend), friend -> Set(person))
  }
  
  test("Should link existing person and friend when added them as friends") {
    val person = "Shankar"
    val friend = "John"
    
    val network: FB.Network = FB.add(Map.empty, person)
    val addPersonNetwork: FB.Network = FB.add(network, friend)
    
    val addedFriendNetwork = FB.friend(addPersonNetwork, person, friend)
    
    addedFriendNetwork should have size 2
    addedFriendNetwork should contain allOf(person -> Set(friend), friend -> Set(person))
  }
  
  test("Should remove person from network") {
    val name = "John"
    val network: FB.Network = FB.add(Map.empty, name)
    val newNetwork: FB.Network = FB.remove(network, name)
    
    newNetwork shouldBe empty
  }
  
  test("Should remove person from network and un-friend them from all their friends") {
    val person = "John"
    val friend = "Amanda"
    val network: FB.Network = FB.add(Map.empty, person)
    val addedFriendNetwork = FB.friend(network, person, friend)
    
    val newNetwork: FB.Network = FB.remove(addedFriendNetwork, person)
    
    newNetwork should have size 1
    newNetwork should contain(friend -> Set.empty)
  }
  
  test("testUnFriend") {
    val person = "John"
    val friend = "Amanda"
    val network: FB.Network = FB.add(Map.empty, person)
    val addedFriendNetwork = FB.friend(network, person, friend)
    
    val newNetwork: FB.Network = FB.remove(addedFriendNetwork, person)
    
    newNetwork should have size 1
    newNetwork should contain(friend -> Set.empty)
    
  }
  
  test("Should friend") {
    val person = "John"
    val friend1 = "Amanda"
    val friend2 = "Shankar"
    val network: FB.Network = FB.add(Map.empty, person)
    val addedFriendNetwork = FB.friend(network, person, friend1)
    val newNetwork = FB.friend(addedFriendNetwork, person, friend2)
    
    newNetwork should have size 3
    newNetwork should contain allOf(
      person -> Set(friend1, friend2),
      friend1 -> Set(person),
      friend2 -> Set(person)
    )
  }
  
  test("Should get the person with most friends") {
    val network: FB.Network =  Map(
      "Shankar" -> Set("Jess", "John"),
      "Jess" -> Set("Shankar", "Thomas", "Monika"),
      "Thomas" -> Set("Jess", "Monika"),
      "Monika" -> Set("Thomas", "Jess"),
      "John" -> Set("Shankar")
    )
    
    val result = FB.getPersonWithMaxFriend(network)
  
    result._1 shouldEqual "Jess"
  }
  
  test("Should get a list of person without any friends") {
    val network: FB.Network =  Map(
      "Shankar" -> Set("Jess"),
      "Jess" -> Set("Shankar", "Thomas", "Monika"),
      "Thomas" -> Set("Jess", "Monika"),
      "Monika" -> Set("Thomas", "Jess"),
      "John" -> Set.empty,
      "Amanda" -> Set.empty
    )
  
    val result = FB.getPersonWithoutFriend(network)
  
    result should have size 2
    result should contain allOf (
      "John" -> Set.empty,
      "Amanda" -> Set.empty
    )
  }
  
  test("Should have social connection") {
    val network: FB.Network =  Map(
      "Shankar" -> Set("Jess", "John"),
      "Jess" -> Set("Shankar", "Monika"),
      "Thomas" -> Set("Jess", "Monika", "Amanda"),
      "Monika" -> Set("Thomas", "Jess"),
      "John" -> Set("Shankar"),
      "Amanda" -> Set("Thomas")
    )
    
    val result = FB.hasSocialConnection(network, "Shankar","Amanda")
    result shouldBe true
  }
  
  test("Shouldn't have social connection") {
    val network: FB.Network =  Map(
      "Shankar" -> Set("Jess", "John"),
      "Jess" -> Set("Shankar", "Monika"),
      "Thomas" -> Set("Jess", "Monika", "Amanda"),
      "Monika" -> Set("Thomas", "Jess"),
      "John" -> Set("Shankar"),
      "Amanda" -> Set("Thomas"),
      "Fernando" -> Set("Alessandra"),
      "Alessandra" -> Set("Fernando")
    )
  
    val result = FB.hasSocialConnection(network, "Shankar","Alessandra")
    result shouldBe false
  }
}
