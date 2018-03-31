import com.thoughtworks.gauge.{Step, Table}

class StepImplementation {
  private var vowels: Set[Char] = _

  @Step(Array("Vowels in English language are <vowelString>."))
  def setLanguageVowels(vowelString: String): Unit =
    vowels = vowelString.toCharArray.toSet

  @Step(Array("The word <word> has <expectedCount> vowels."))
  def verifyVowelsCountInWord(word: String, expectedCount: Int): Unit = {
    val actualCount = countVowels(word)
    assert(expectedCount == actualCount)
  }

  @Step(Array("Almost all words have vowels <wordsTable>"))
  def verifyVowelsCountInMultipleWords(wordsTable: Table): Unit =
    wordsTable.getTableRows.forEach { row =>
      val word = row.getCell("Word")
      val expectedCount = row.getCell("Vowel Count").toInt
      val actualCount = countVowels(word)
      assert(expectedCount == actualCount)
    }

  private def countVowels(word: String): Int =
    word.count(vowels.contains)
}