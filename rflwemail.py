import smtplib

#sendemail(temperature value, recipient email, state number)
def sendemail(temp, recip, state):
    FRM = ['REFLOW OVEN CONTROLLER']
    TO = recip
    SBJ = 'Reflow Oven Message'
    if state == 1:
        statename = 'ramp'
    elif state == 2:
        statename = 'soak'
    elif state == 3:
        statename = 'peak'
    elif state == 4:
        statename = 'reflow'
    elif state == 5:
        statename = 'cooling'
    CNT = ('Your oven has reached ' + str(temp) + 'C and is in state: ' + statename + '.\n\n\nsent using python')
    MSG = """\From: %s\nTo: %s\nSubject: %s\n\n%s""" % (FRM, ", ".join(TO), SBJ, CNT)
    
    USR = 'elec291project1python@gmail.com'
    PWD = 'pythonemailing'
    
    try:
        server = smtplib.SMTP_SSL("smtp.gmail.com", 465)
        server.ehlo()
        server.login (USR, PWD)
        
        server.sendmail(FRM, TO, MSG)
        server.close()
        print 'sent'
        return True
    except:
        print 'failed'
        return False
