Web Scrapper to extract Java Language Specification web page to a file, which will be used as an input for AST generation.

Beautiful Soup in python is used to scrap the main structure of the web pages.
Web Page Class Structure is as follows:

->productionset
-->productionrecap
--->production
---->lhs
---->rhs

The html content within rhs does not folow any particular structure, so its parsed manually using regular expressions. The entities between the tags are extracted and appended to the output file. If a <br> tag is encountered, then a corresponding new line is added to the output file.

Special cases:
1) when a line contains (one of), we directly write it to the output file.
2) html_escape_table is a hash table to map html escape sequences to the actual symbols.
3) if the tag contains <div> tag and it contains "StatementExpressionList" word, then the word is append to the result set,
else the entire tag is skipped.
4) The productions for "Lexical Structure" block is skipped since the entities are not required while AST generation.

Input: page_link of the java language specification source as command line argument
	Example: page_link = 'https://docs.oracle.com/javase/specs/jls/se12/html/jls-19.html'
Output: The file with extracted html page contents
	Example: outputFile = open("./javaGrammar.txt", "w+")
	    The output will also be printed to the console.
