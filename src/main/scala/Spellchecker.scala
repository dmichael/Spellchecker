import collection.mutable.ArrayBuffer

object Spellchecker {
    val dictionary = Node('\0')

    def buildDictionary(words: Array[String]) {
        words.foreach(word => fill(dictionary.children, word))
    }

    def fill(nodes: ArrayBuffer[Node], letters: String){
        val (head, tail) = (letters.head, letters.tail)

        val node = nodes.find(_.value == head) match {
            case Some(node) => node
            case None => {
                val node = Node(head)
                (nodes += node ).last
            }
        }

        // recursive call
        if(!tail.isEmpty) {
            fill(node.children, tail)
        }
    }
}

class Spellchecker {
    def check(word: String): Boolean = exists(Spellchecker.dictionary.children, word.trim)

    def exists(nodes: ArrayBuffer[Node], letters: String): Boolean = {
        val (head, tail) = (letters.head, letters.tail)

        nodes.find( _.value == head ) match {
            case Some(node) if !tail.isEmpty => exists(node.children, tail)
            case None => false
            case _ => true
        }
    }
}