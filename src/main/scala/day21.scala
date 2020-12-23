package day21

case class Food(val ingredients: Set[String], val allergens: Set[String]);

object Food {
  def apply(input: String) = {
    val parts = input
      .replaceAll("\\)", "")
      .split(" \\(contains ")

    val ingredients = parts(0).split(" ").toSet
    val allergens = parts(1).split(", ").toSet

    new Food(ingredients, allergens)
  }
}

class FoodCollection(foods: List[Food]) {
  val allergens   = foods.map(_.allergens).reduce((a, b) => a | b)
  val ingredients = foods.map(_.ingredients).reduce((a, b) => a | b)

  def potentialMap() : Map[String, Set[String]] = {
    var potential = allergens
      .map(allergen => (allergen -> ingredients))
      .toMap

    foods.foreach(food => {
      food.allergens.foreach(allergen => {
        potential += (allergen -> (potential(allergen) & food.ingredients))
      })
    })

    potential
  }

  def potentialAllergens() = potentialMap().values.reduce((a, b) => a | b)
  def notAllergens() = ingredients -- potentialAllergens()
  def notAllergenCount() : Int = {
    val not = notAllergens()
    foods.map(food => (food.ingredients & not).size).sum
  }

  def allergenMap() : Map[String, String] = {
    var potential = potentialMap()

    while (potential.exists { case (key, set) => set.size > 1 }) {
      val singles = potential.filter { case (allergen, ingredients) => ingredients.size == 1 }
      val resolved = singles.values.reduce((a, b) => a | b).toSet

      potential.foreach { case (allergen, ingredients) =>
        if (ingredients.size > 1) {
          potential = potential + (allergen -> (ingredients -- resolved))
        }
      }

    }

    potential.map { case (k, v) => (v.head -> k) }.toMap
  }

  def dangerousList : String = {
    allergenMap()
      .map { case (ingredient, allergen) => (ingredient, allergen) }
      .toList
      .sortBy { case (i, a) => a }
      .map { case (i, _) => i }
      .mkString(",")
  }
}

object FoodCollection {
  def apply(input: Iterator[String]) = new FoodCollection(input.map(i => Food(i)).toList)
}
