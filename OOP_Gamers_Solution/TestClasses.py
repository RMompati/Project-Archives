from gamers.ProGamer import ProGamer
from gamers.AmGamer import AmGamer

def print_pro(code, id, names, years, rank, r_fee, discount):
    gamer_info = f"{code:<10}\t{id:<10}\t{names:<20}"
    gamer_info += f"\t{years:>4}\t{rank:>4}\t{r_fee:>18}"
    gamer_info += f"\t{discount:>11}"
    print(gamer_info)


def print_am(code, id, names, category, r_fee, discount):
    gamer_info = f"{code:<10}\t{id:<10}\t{names:<20}"
    gamer_info += f"\t{category:>8}\t{r_fee:>18}"
    gamer_info += f"\t{discount:>11}"
    print(gamer_info)

def main():

    proGamers = [
        ProGamer('840725', 'Dell', 'Aio', 'P', 12, 56),
        ProGamer('950826', 'Cpu', 'Evetec', 'P', 8, 34)
    ]

    amGamers = [
        AmGamer('021206', 'Samsung', '32In', 'A', 2),
        AmGamer('921116', '4TB', 'Transend', 'A', 3)
    ]

    print("Professional Gamers")
    print("Code\t\tID Number\tNames\t\t\tYears\tRank\tRegistration fee(R)\tDiscount(R)")

    for gamer in proGamers:
        print_pro(gamer.generateCode(), gamer.getIdNumber(), gamer.getNames(),
                     gamer.getYears(), gamer.getRank(), gamer.registrationFee(),
                     gamer.discountAmount()
                     )

    print("\nAmateur Gamers")
    print("Code\t\tID Number\tNames\t\t\tCategory\tRegistration fee(R)\tDiscount(R)")
    for gamer in amGamers:
        print_am(gamer.generateCode(), gamer.getIdNumber(), gamer.getNames(),
                     gamer.getCategory(), gamer.registrationFee(),
                     gamer.discountAmount()
                     )


if __name__ == '__main__':
    main()
