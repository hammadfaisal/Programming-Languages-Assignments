#Markdown to HTML converter


Implemented a markdown to HTML converter in SML which supports a subset of markdown syntax including bold, italics, underline, links, headers, ordered and unordered lists, blockquotes, code blocks, horizontal rules, and tables.

In code blocks, the indentation gets reduced by 4 spaces even in case of codeblock inside lists which looks
different from the code block on assignment page.

Blockquotes and both kinds of lists can be nested and used within each other too.
Text in table cells have the same features as any other inline text like bolds, italics, underline,
and links. A line starting with 3 consecutive hyphens is treated as a horizontal rule and any text in 
it is ignored.

Lists are considered till a new line followed by non-list line with no indentation or End of File.
Unlike lists, only lines starting with > are considered part of blockquote.


## **Errors Handled**

- Mismatch in asterisks or redundant asterisks
- Links not having a closing bracket
- Label having no corresponding link
- Invalid number of hashes for header