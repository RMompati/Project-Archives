"""
    @author Patco Erold
"""

from abc import ABCMeta, abstractmethod

class GamerInt(metaclass = ABCMeta):

    @classmethod
    def __subclasshook__(cls, subclass):
        return (hasattr(subclass, 'discountAmount') and
                callable(subclass.discountAmount) and
                hasattr(subclass, 'registrationFee') and
                callable(subclass.registrationFee) or
                NotImplemented)
    
    @abstractmethod
    def discountAmount(self):
        pass

    @abstractmethod
    def registrationFee(self):
        pass