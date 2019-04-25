from bs4 import BeautifulSoup
import requests
import re

html_escape_table = {
	"&amp;": "&",
	"&quot;": '"',
	"&apos;": "'",
	"&gt;": ">",
	"&lt;": "<"
}


def translateHTMLCodes(result):
	resString = result.splitlines()
	for line in resString:
		for key in html_escape_table:
			if (line.__contains__(key)):
				line = line.replace(key, html_escape_table[key])
		#print(line)
		outputFile.writelines(line + "\n")


def process(rhs):
	# convert rhs from bs4.element.tag to string
	result = ""
	rhString = str(rhs).splitlines()
	brFlag = False
	for line in rhString:
		if (line.__contains__("div")):
			if (line.__contains__("StatementExpressionList")):
				result += "StatementExpressionList"
			continue;
		if (line.__contains__("one of")):
			result += "(one of)"
		else:
			line = line.strip()
			regex = re.compile("<\w*[^<>]*>|<\/\w*\s*>")
			output = regex.split(line)
			for entry in output:
				result += entry
		if (line.__contains__("br")):
			result += "\n"
		result += " "

	result = result.strip()
	result = " " + result
	translateHTMLCodes(result)


if __name__ == '__main__':
	page_link = 'https://docs.oracle.com/javase/specs/jls/se12/html/jls-19.html'
	# outputFile to write output
	outputFile = open("./javaGrammar-se12.txt", "w+")
	page_response = requests.get(page_link, timeout=5)
	soup = BeautifulSoup(page_response.content, "html.parser")
	prodset = soup.find_all('div', attrs={'class': 'productionset'})

	for ps in prodset:
		prodrecap = ps.find_all('div', attrs={'class': 'productionrecap'})
		for pr in prodrecap:
			prod = pr.find('div', attrs={'class': 'production'})
			lhs = prod.find('div', attrs={'class': 'lhs'})
			#print(lhs.get_text())
			outputFile.writelines(str(lhs.get_text()) + "\n")
			rhs = prod.find('div', attrs={'class': 'rhs'})
			rhsProcessed = process(rhs)
			outputFile.writelines("\n")
			#print()
	# break;
	# break;
	outputFile.close()