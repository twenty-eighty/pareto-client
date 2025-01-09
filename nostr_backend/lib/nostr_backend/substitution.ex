defmodule NostrBackend.Substitution do
  @substitutions [
    {"A", ["Α", "А"]},
    {"Ä", ["Ӓ"]},
    {"B", ["Β", "В"]},
    {"°C", ["℃"]},
    {"C", ["С", "Ϲ", "Ⅽ"]},
    {"Ç", ["Ҫ"]},
    {"D", ["Ⅾ"]},
    {"TEL", ["℡"]},
    {"TM", ["™"]},
    {"E", ["Ε", "Е"]},
    {"°F", ["℉"]},
    {"F", ["Ϝ"]},
    {"G", ["Ԍ"]},
    {"H", ["Η", "Н"]},
    {"IJ", ["Ĳ"]},
    {"IO", ["Ю"]},
    {"II", ["Ⅱ"]},
    {"III", ["Ⅲ"]},
    {"IV", ["Ⅳ"]},
    {"VI", ["Ⅵ"]},
    {"VII", ["Ⅶ"]},
    {"VIII", ["Ⅷ"]},
    {"IX", ["Ⅸ"]},
    {"XI", ["Ⅺ"]},
    {"XII", ["Ⅻ"]},
    {"I", ["Ι", "І"]},
    {"J", ["Ј"]},
    {"K", ["Κ", "К", "K"]},
    {"L", ["Ⅼ"]},
    {"M", ["Μ", "М", "Ϻ", "Ⅿ"]},
    {"N", ["Ν"]},
    {"OE", ["Œ"]},
    {"O", ["Ο", "О"]},
    {"Ö", ["Ӧ"]},
    {"P", ["Ρ", "Р"]},
    {"Q", ["Ԛ"]},
    {"S", ["Ѕ"]},
    {"T", ["Τ", "Т"]},
    {"V", ["Ѵ", "Ⅴ"]},
    {"W", ["Ԝ"]},
    {"X", ["Χ", "Х", "Ⅹ"]},
    {"Y", ["Υ", "Ү"]},
    {"Z", ["Ζ"]},
    {"ae", ["æ", "ӕ"]},
    {"a", ["а"]},
    {"ä", ["ӓ"]},
    {"c/o", ["℅"]},
    {"c/u", ["℆"]},
    {"c", ["с"]},
    {"d", ["ԁ", "ⅾ"]},
    {"oe", ["œ"]},
    {"e", ["е"]},
    {"è", ["ѐ"]},
    {"ë", ["ё"]},
    {"fi", ["ﬁ"]},
    {"fl", ["ﬂ"]},
    {"g", ["ɡ"]},
    {"ij", ["ĳ"]},
    {"ii", ["ⅱ"]},
    {"iii", ["ⅲ"]},
    {"iv", ["ⅳ"]},
    {"vi", ["ⅵ"]},
    {"vii", ["ⅶ"]},
    {"viii", ["ⅷ"]},
    {"ix", ["ⅸ"]},
    {"xi", ["ⅺ"]},
    {"xii", ["ⅻ"]},
    {"i", ["і"]},
    {"j", ["ϳ", "ј"]},
    {"m", ["ⅿ"]},
    {"o", ["ο", "о"]},
    {"ó", ["ό"]},
    {"p", ["р"]},
    {"q", ["ԛ"]},
    {"s", ["ѕ"]},
    {"v", ["ⅴ"]},
    {"w", ["ԝ"]},
    {"x", ["х", "ⅹ"]},
    {"y", ["у"]},
    {"ß", ["β"]},
    {"1/3", ["⅓"]},
    {"1/5", ["⅕"]},
    {"1/6", ["⅙"]},
    {"1/8", ["⅛"]},
    {"1/", ["⅟"]},
    {"2/3", ["⅔"]},
    {"2/5", ["⅖"]},
    {"3/5", ["⅗"]},
    {"3/8", ["⅜"]},
    {"4/5", ["⅘"]},
    {"5/6", ["⅚"]},
    {"5/8", ["⅝"]},
    {"7/8", ["⅞"]},
    {"<<", ["≪"]},
    {">>", ["≫"]},
    {"<=", ["≤"]},
    {">=", ["≥"]},
    {"=>", ["⇒"]},
    {"<-", ["←"]},
    {"->", ["→"]},
    {"--", ["—"]},
    {"-", ["‐"]},
    {"/", ["⁄", "∕"]},
    {"\\", ["∖"]},
    {",", ["‚"]},
    {"...", ["…"]},
    {"*", ["∗"]}
  ]

  @doc """
  Replaces words or characters in the text with their look-alike substitutions.

  - First, attempts to replace entire words (e.g., "VII").
  - If no word match is found, substitutes characters individually if possible.
  """
  def replace_randomly(nil), do: nil

  def replace_randomly(text) do
    text
    |> substitute_words()
    |> substitute_characters()
  end

  defp substitute_words(text) do
    Enum.reduce(@substitutions, text, fn {key, values}, acc ->
      if String.contains?(acc, key) do
        replacement = Enum.random(values)
        String.replace(acc, ~r/\b#{Regex.escape(key)}\b/, replacement, global: true)
      else
        acc
      end
    end)
  end

  defp substitute_characters(text) do
    String.graphemes(text)
    |> Enum.map(&substitute_character/1)
    |> Enum.join()
  end

  defp substitute_character(char) do
    case Enum.find(@substitutions, fn {key, _} -> key == char end) do
      nil -> char
      {_, values} -> Enum.random(values)
    end
  end
end
