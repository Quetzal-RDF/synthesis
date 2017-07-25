from bs4 import BeautifulSoup
import urllib.request
import re


def extractFunctions(requestURL, tableID, id1, id2):
    request = urllib.request.Request(requestURL)
    response = urllib.request.urlopen(request)
    page = response.read()

    soup = BeautifulSoup(page, 'html.parser')

    h = soup.find("a", {"id": tableID})
    table = h.find_next_siblings("table")[0]
    rows = table.tbody.find_all('tr')
    for row in rows:
        cells = row.find_all('td')
        txt = cells[id1].get_text().replace('\n', ' ')
        txt = re.sub('[(][^)]*[)]',' ', txt)
        doc = txt + " " + cells[id2].get_text().replace('\n', ' ')
        print(re.sub(' +',' ',doc))


extractFunctions('https://www.postgresql.org/docs/9.6/static/functions-string.html', "FUNCTIONS-STRING-SQL", 0, 2)
extractFunctions('https://www.postgresql.org/docs/9.6/static/functions-string.html', "FUNCTIONS-STRING-OTHER", 0, 2)
extractFunctions('https://www.postgresql.org/docs/9.6/static/functions-math.html', "FUNCTIONS-MATH-OP-TABLE", 0, 1)
extractFunctions('https://www.postgresql.org/docs/9.6/static/functions-math.html', "FUNCTIONS-MATH-FUNC-TABLE", 0, 2)
extractFunctions('https://www.postgresql.org/docs/9.6/static/functions-math.html', "FUNCTIONS-MATH-RANDOM-TABLE", 0, 2)
extractFunctions('https://www.postgresql.org/docs/9.6/static/functions-math.html', "FUNCTIONS-MATH-TRIG-TABLE", 0, 2)
extractFunctions('https://www.postgresql.org/docs/9.6/static/functions-comparison.html', 'FUNCTIONS-COMPARISON-OP-TABLE', 0, 1)
extractFunctions('https://www.postgresql.org/docs/9.6/static/functions-comparison.html', 'FUNCTIONS-COMPARISON-FUNC-TABLE', 0, 1)
extractFunctions('https://www.postgresql.org/docs/9.6/static/functions-datetime.html', 'FUNCTIONS-DATETIME-TABLE', 0, 2)







