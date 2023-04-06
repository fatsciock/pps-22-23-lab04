package u04lab.code

import u04lab.code.List.*
import u04lab.code.Option.*

class WarehouseImpl extends Warehouse {

  private var inventory: List[Item] = empty

  override def store(item: Item): Unit = if !contains(item.code) then
    inventory = cons(item, inventory)

  override def contains(itemCode: Int): Boolean = !Option.isEmpty(find(inventory)(_.code == itemCode))

  override def searchItems(tag: String): List[Item] = filter(inventory)(i => List.contains(i.tags, tag))

  override def retrieve(code: Int): Option[Item] = find(inventory)(_.code == code)

  override def remove(item: Item): Unit = inventory = filter(inventory)(_ != item)
}
