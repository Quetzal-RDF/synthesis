from bs4 import BeautifulSoup
import urllib.request
import re


def extractFunctions(requestURL, tableID, id1, id2):
    request = urllib.request.Request(requestURL)
    response = urllib.request.urlopen(request)
    page = response.read()

    soup = BeautifulSoup(page, 'html.parser')

    tables = soup.find_all("table", {"class": tableID})
    for table in tables:
        rows = table.find_all('tr')
        for row in rows:
            cells = row.find_all('td')
            if (len(cells) == 0):
                continue
            txt = cells[id1].get_text().replace('\n', ' ')
            txt = re.sub('[(][^)]*[)]',' ', txt)
            doc = txt + " " + cells[id2].get_text().replace('\n', ' ')
            print(re.sub(' +',' ',doc))


extractFunctions('https://www.w3schools.com/sql/sql_ref_msaccess.asp', "w3-table-all notranslate", 0, 1)
extractFunctions('https://www.w3schools.com/sql/sql_ref_sqlserver.asp', "w3-table-all notranslate", 0, 1)
extractFunctions('https://www.w3schools.com/sql/sql_ref_oracle.asp', "w3-table-all notranslate", 0, 1)







