object Q1 {
  // Define the product case class
  case class Product(name: String, quantity: Int, price: Double)

  // Sample inventories
  var inventory1: Map[Int, Product] = Map(
    101 -> Product("ProductA", 10, 5.99),
    102 -> Product("ProductB", 20, 15.49),
    103 -> Product("ProductC", 15, 7.99)
  )

  var inventory2: Map[Int, Product] = Map(
    102 -> Product("ProductB", 5, 18.00),
    104 -> Product("ProductD", 7, 12.49)
  )

  // I. Retrieve all product names from inventory1
  def retrieveProductNames(inventory: Map[Int, Product]): List[String] = {
    inventory.values.map(_.name).toList
  }

  // II. Calculate the total value of all products in inventory1
  def calculateTotalValue(inventory: Map[Int, Product]): Double = {
    inventory.values.map(product => product.quantity * product.price).sum
  }

  // III. Check if inventory1 is empty
  def isInventoryEmpty(inventory: Map[Int, Product]): Boolean = {
    inventory.isEmpty
  }

  // IV. Merge inventory1 and inventory2, updating quantities and retaining the highest price
  def mergeInventories(inv1: Map[Int, Product], inv2: Map[Int, Product]): Map[Int, Product] = {
    inv2.foldLeft(inv1) { case (acc, (id, product)) =>
      acc.get(id) match {
        case Some(existingProduct) =>
          acc + (id -> Product(
            name = existingProduct.name,
            quantity = existingProduct.quantity + product.quantity,
            price = Math.max(existingProduct.price, product.price)
          ))
        case None => acc + (id -> product)
      }
    }
  }

  // V. Check if a product with a specific ID (e.g., 102) exists and print its details
  def checkProductExists(inventory: Map[Int, Product], productId: Int): Unit = {
    inventory.get(productId) match {
      case Some(product) => println(s"Product ID: $productId, Details: $product")
      case None => println(s"Product with ID $productId does not exist.")
    }
  }

  // Test the functions
  def main(args: Array[String]): Unit = {
    println("Product Names in Inventory1: " + retrieveProductNames(inventory1))
    println("Total Value of Inventory1: $" + calculateTotalValue(inventory1))
    println("Is Inventory1 Empty: " + isInventoryEmpty(inventory1))
    
    val mergedInventory = mergeInventories(inventory1, inventory2)
    println("Merged Inventory: " + mergedInventory)

    checkProductExists(inventory1, 102)
  }
}
