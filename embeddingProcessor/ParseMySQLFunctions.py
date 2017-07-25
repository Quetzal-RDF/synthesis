from bs4 import BeautifulSoup
import urllib.request
import re


def extractFunctions(requestURL, tableID, id1, id2):
    request = urllib.request.Request(requestURL)
    response = urllib.request.urlopen(request)
    page = response.read()

    soup = BeautifulSoup(page, 'html.parser')

    table = soup.find("table", {"summary": tableID})
    rows = table.tbody.find_all('tr')
    for row in rows:
        cells = row.find_all('td')
        txt = cells[id1].get_text().replace('\n', ' ')
        txt = re.sub('[(][^)]*[)]',' ', txt)
        doc = txt + " " + cells[id2].get_text().replace('\n', ' ')
        print(re.sub(' +',' ',doc))


extractFunctions('https://dev.mysql.com/doc/refman/8.0/en/string-functions.html', "String Operators", 0, 1)
extractFunctions('https://dev.mysql.com/doc/refman/8.0/en/numeric-functions.html', "Numeric Functions and Operators", 0, 1)
extractFunctions('https://dev.mysql.com/doc/refman/8.0/en/arithmetic-functions.html', "Arithmetic Operators", 0, 1)
extractFunctions('https://dev.mysql.com/doc/refman/8.0/en/date-and-time-functions.html', "Date/Time Functions", 0, 1)








