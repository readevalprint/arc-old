import imaplib
from pprint import pprint
mail = imaplib.IMAP4_SSL('imap.gmail.com')
mail.login("tim@bubblefresh.com","toomba")
typ, data = mail.list()
pprint(data)
mail.select("INBOX")
typ, data = mail.search(None, '(ALL)')
#pprint(data)
res = data[0].split()
if res:
  last = res[-1]
  pprint(last)
  pprint(mail.fetch(last, '(RFC822)'))
else:
  print "No mail"
