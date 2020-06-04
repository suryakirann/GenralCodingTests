import org.scalatest.FunSuite

class TailRecursionCodeTest extends FunSuite {
  test("Test-1: Counting the frequency of characters") {
    assert(
      TailRecursionCode.countConsCharFrequency(
        "AAAAABBBCCC") == "5A3B3C")

  }

  test("Test-2: Ignore numbers for single  characters") {
    assert(
      TailRecursionCode.countConsCharFrequency(
        "AAAAABBBCCCD") === "5A3B3CD")
  }
}
