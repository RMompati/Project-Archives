from . import Gamer as g

class ProGamer(g.Gamer):

    __years = 0
    __rank = 0

    DISCOUNT_PERCENT_1 = 0.005
    DISCOUNT_PERCENT_2 = 0.5
    REGISTRATION_FEE  = 500

    def __init__(self, idNumber: str, firstName: str, lastName: str, status: str, years: int, rank:int):
        super().__init__(idNumber, firstName, lastName, status)
        self.setYears(years)
        self.setRank(rank)

    def setYears(self, years):
        self.__years = years

    def setRank(self, rank):
        self.__rank = rank

    def getYears(self):
        return self.__years

    def getRank(self):
        return self.__rank

    def generateCode(self):
        return f"{ self.getStatus() }{ self.getRank() }{ self.calculateAge() }-{ self.getYears() }"

    def discountAmount(self):
        amount = 00.00

        if self.getYears() < 10 :
            amount = self.REGISTRATION_FEE * (self.DISCOUNT_PERCENT_1 * self.getYears())
        else:
            amount =  self.REGISTRATION_FEE * self.DISCOUNT_PERCENT_2
        return amount

    def registrationFee(self):
        amount = self.REGISTRATION_FEE - self.discountAmount()
        return amount