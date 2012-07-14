import org.scalatest.FunSpec
import org.scalatest.BeforeAndAfterAll

class SpellcheckerTest extends FunSpec with BeforeAndAfterAll {
    val words = Array("once", "upon", "a", "time", "in", "a", "galaxy", "far", "far", "away")
    Spellchecker.buildDictionary(words)
    val spellchecker = new Spellchecker()

    // tests go here...
    describe("successful matches") {
        it("should match full words"){
            assert(spellchecker check "upon")
            assert(spellchecker check "time")
            assert(spellchecker check "galaxy")
        }

        it("should match partial words"){
            assert(spellchecker check "o")
            assert(spellchecker check "ti")
            assert(spellchecker check "upo")
        }

        it("should match words with spaces before or after"){
            assert(spellchecker check "upon ")
            assert(spellchecker check " upon ")
            assert(spellchecker check " upon")
        }
    }

    describe("unsuccessful matches") {
        it("should not match words not in the dictionary") {
            assert(spellchecker.check("howdy") === false)
            assert(spellchecker.check("upu") === false)
        }
    }
}