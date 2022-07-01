from .GamerInt import GamerInt
from datetime import date
from abc import ABC, abstractmethod

class Gamer(ABC, GamerInt):
    __idNumer = 000000
    __firstName = None
    __LastName = None
    __status = None

    def __init__(self, idNumber:str, firstName: str, lastName: str, status: str):
        self.setIdNumber(idNumber)
        self.setFirstName(firstName)
        self.setLastName(lastName)
        self.setStatus(status)
        super(Gamer, self).__init__()

    def setIdNumber(self, idNumber):
        self.__idNumer = idNumber

    def setFirstName(self, firstName):
        self.__firstName = firstName

    def setLastName(self, lastName):
        self.__LastName = lastName

    def setStatus(self, status):
        self.__status = status

    def getIdNumber(self):
        return self.__idNumer

    def getFirstName(self):
        return self.__firstName

    def getLastName(self):
        return self.__LastName

    def getNames(self):
        return f"{self.getFirstName()} {self.getLastName()}"

    def getStatus(self):
        return self.__status

    @abstractmethod
    def generateCode(self):
        raise NotImplementedError

    def discountAmount(self):
        return 0.0

    def registrationFee(self):
        return 00.00

    def calculateAge(self):
        age = 0

        today = date.today()
        date_of_birth = None

        if self.getIdNumber()[0] == 0:
            date_of_birth = '20' + self.getIdNumber()
        else:
            date_of_birth = '19' + self.getIdNumber()

        date_of_birth = date(int(date_of_birth[0:4]), int(date_of_birth[4:6]), int(date_of_birth[6:8]))

        age = today.year - date_of_birth.year - ((today.month, today.day) < (date_of_birth.month, date_of_birth.day))

        return age