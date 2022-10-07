#%%

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

insurance = pd.read_excel("guia_1_insurance.xlsx")
# print(insurance)


#%%

print(insurance.shape)
print(insurance.head)
print(insurance.tail)

#%%

children = insurance["children"]
insurance = pd.read_excel("guia_1_insurance.xlsx")
insurance = insurance.rename({"age":"edad", "sex":"sexo", "charges":"gastos"},axis=1)
sex_ = {"male":"hombre", "female":"mujer"}
insurance["sexo"] = insurance["sexo"].map(sex_)
# print(insurance)

sns.scatterplot(insurance["edad"], insurance["gastos"], hue=insurance["sexo"], palette=["purple", "#55CCCC"], s=children*30, alpha=0.7).set_title("Gastos por edad dependiendo de sexo e hijos")