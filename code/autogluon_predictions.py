import pyreadr
import pandas as pd
import numpy as np
import seaborn as sns
import sys
from autogluon.tabular import TabularPredictor

model_path = sys.argv[1]
output_file = sys.argv[2]


df_ano= pyreadr.read_r("data/prb/beta_complete_test_bmiq.Rdata")
#predictor =\
#TabularPredictor.load("data/redhat_2/AutogluonModels/ag-20241006_031312")

predictor = TabularPredictor.load(model_path)


test_df =df_ano['beta_norm_BMIQ']
test_df=test_df.transpose()

predictions = predictor.predict(test_df,as_pandas=True)
predictions=pd.DataFrame(predictions)

predictions.columns=["GA"]
predictions['Sample_ID'] = predictions.index.values
predictions.to_csv(output_file,index=False)
