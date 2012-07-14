/**
 * A simple class that can act as a spellchecker
 * - could be extended to autocomplete
 *
 * The basic principle here is that words forming the dictionary are stored in a tree structure,
 * with unique values [a-z] at each node. Children of each node are also unique in the set [a-z].
 * In this way, words are stored by traversing common keys down the branches until the final leaf
 * is found (or not as the case may be).
 *
 * Finding the word once the tree is constructed is essentially a breadth-first search through each layer
 * of the tree
 *
 * I believe this is called a trie
 * It should look something like this:
 *           s
 *          / \
 *         a   o
 *        /\    \
 *       v  f    f
 *      /    \    \
 *     e      e    a
 *
 *  TODO: Make all structures immutable. Specifically, change ArrayBuffer into List if possible.
 *  TODO: Convert to HashMaps instead of ArrayBuffers! More efficient lookup
 */

import collection.mutable.ArrayBuffer

object Spellchecker {
    // We start with null root node ...
    val dictionary = Node('\0')

    def buildDictionary(words: Array[String]) {
        words.foreach(word => fill(dictionary.children, word))
    }

    /**
     * Called recursively to build up a tree representing the dictionary
     */
    def fill(nodes: ArrayBuffer[Node], letters: String){
        val (head, tail) = (letters.head, letters.tail)

        // If the node for this letter does not already exist,
        // create it and add it to the others
        val node = nodes.find(_.value == head) match {
            case Some(node) => node
            case None => {
                val node = Node(head)
                (nodes += node).last
            }
        }

        if(!tail.isEmpty) {
            fill(node.children, tail)
        }
    }
}

class Spellchecker {

    def check(word: String): Boolean = exists(Spellchecker.dictionary.children, word.trim)

    /**
     * Called recursively, walking the tree as nodes are found at each layer looking for matches
     */
    private def exists(nodes: ArrayBuffer[Node], letters: String): Boolean = {
        val (head, tail) = (letters.head, letters.tail)

        nodes.find( _.value == head ) match {
            case Some(node) if !tail.isEmpty => exists(node.children, tail)
            case None => false
            case _ => true
        }
    }
}