import pandas as pd
import matplotlib.pyplot as plt

def main():
    for s in stats:
        s.plot()
        plt.grid()

    plt.show()

if __name__ == "__main__":
    stats = [pd.read_csv(f"dataFn{i}.csv").set_index("n") for i in range(3)]
    did_it = [s[s['sucesso'] == 1][["maximo","media","variancia"]] for s in stats]
    did_not = [s[s['sucesso'] == 0][["maximo","media","variancia"]] for s in stats]
