import pyreadr
import pandas as pd
import numpy as np
import seaborn as sns
from autogluon.tabular import TabularPredictor

df_ano= pyreadr.read_r("data/prb/beta_raw.Rdata")
predictor =\
TabularPredictor.load("data/grid_cpu_1/AutogluonModels/ag-20240926_021756")



test_df =df_ano['beta_test']
test_df=test_df.transpose()

predictions = predictor.predict(test_df,as_pandas=True)
predictions=pd.DataFrame(predictions)

predictions.columns=["GA"]
predictions['Sample_ID'] = predictions.index.values
predictions.to_csv("data/processed/autogluon_predictions.csv",index=False)
