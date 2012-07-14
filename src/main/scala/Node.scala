import collection.mutable.ArrayBuffer

object Node {
    def apply(value: Char, children: ArrayBuffer[Node] = ArrayBuffer.empty[Node]) = new Node(value, children)
}

class Node(val value: Char, val children: ArrayBuffer[Node])

