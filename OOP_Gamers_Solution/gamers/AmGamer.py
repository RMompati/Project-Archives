from .Gamer import Gamer
import random

class AmGamer(Gamer):

    __category = 0


    DISCOUNT_PERCENT = 0.20
    REGISTRATION_FEE  = 250

    def __init__(self, idNumber: str, firstName: str, lastName: str, status: str, category: int):
        super().__init__(idNumber, firstName, lastName, status)
        self.setCategory(category)


    def setCategory(self, category):
        self.__category = category

    def getCategory(self):
        return self.__category

    def generateCode(self):

        digits = '%02d'%random.randint(0, 99)
        code = ''

        try:
            if int(digits) < 10:
                code = f"{self.getStatus() }0{ digits }{ self.calculateAge() }-{ self.getCategory() }"
            else:
                code = f"{self.getStatus() }{ digits }{ self.calculateAge() }-{ self.getCategory()}"
        except Exception as e:
            print(e)
        return code

    def discountAmount(self):

        amount = 00.00

        amount = self.REGISTRATION_FEE * self.DISCOUNT_PERCENT
        return amount

    def registrationFee(self):
        return (self.REGISTRATION_FEE - self.discountAmount())