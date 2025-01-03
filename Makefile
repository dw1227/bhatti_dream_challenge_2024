results/rankings_leaderboard_test.pptx: code/run_r_script.sh \
	            data/submissions/Job-393694313420778661233189284.csv \
							data/submissions/Test_data_evaluation.csv \
							code/run_r_script.sh\
							code/get_ranks_leaderboard_test.R	                               
	code/run_r_script.sh code/get_ranks_leaderboard_test.R 


data/processed/autogluon_predictions_450k.csv: \
			data/prb/beta_complete_test_bmiq.Rdata\
			data/automl/redhat_2/AutogluonModels/ag-20241006_031312/ \
			code/run_python_script.sh\
			code/autogluon_predictions.py
	code/run_python_script.sh code/autogluon_predictions.py data/automl/redhat_2/AutogluonModels/ag-20241006_031312/ $@
	        
data/processed/autogluon_predictions_850k.csv: \
			data/prb/beta_complete_test_bmiq.Rdata \
			data/automl/autog850_360_cpu/ag-20241021_160023/ \
			code/run_python_script.sh\
			code/autogluon_predictions.py
	code/run_python_script.sh code/autogluon_predictions.py data/automl/autog850_360_cpu/ag-20241021_160023/ $@

	        
data/processed/autogluon_predictions_850k_2.csv: \
			data/prb/beta_complete_test_bmiq.Rdata \
			data/automl/autog360_norm_4days/ag-20241025_220439/ \
			code/run_python_script.sh\
			code/autogluon_predictions.py
	code/run_python_script.sh code/autogluon_predictions.py data/automl/autog360_norm_4days/ag-20241025_220439/ $@
	
	
	        
data/clocks/wsu_pl_clock_450k.Rdata: \
			data/dream_challenge/beta_public_normalized_sc1.Rdata \
			code/wsu_placenta_clock_450k.R \
			code/submit_wsu_placenta_clock_training.sh
	sbatch code/submit_wsu_placenta_clock_training.sh
	
	

data/processed/ano_all_predictions.csv: data/prb/beta_complete_test_bmiq.Rdata\
              data/processed/autogluon_predictions_450k.csv\
			  data/processed/autogluon_predictions_850k.csv\
              data/prb/anoall.csv\
              data/submissions/Job-393694313420778661233189284.csv\
              data/submissions/Test_data_evaluation.csv\
						  code/run_r_script.sh\
						  code/get_all_predictions.R                             
	code/run_r_script.sh code/get_all_predictions.R
	
	

	
results/demographics_test.csv results/Test_training_datasets_GA_distribution.pdf: \
              data/prb/sample_metadata.csv\
						  data/prb/anoall.csv\
						  code/run_r_script.sh\
						  code/get_demographics_table_ga_dist.R                            
	code/run_r_script.sh code/get_demographics_table_ga_dist.R	
	
	
	
results/parity_plots.pdf: data/submissions/Test_data_evaluation.csv\
						  data/processed/ano_all_predictions.csv\
						  code/run_r_script.sh\
						  code/generate_parity_plots_predictions.R                             
	code/run_r_script.sh code/generate_parity_plots_predictions.R	
	
results/Bootstrap_RMSE_Results.csv: code/run_r_script.sh \
	            data/submissions/Job-393694313420778661233189284.csv \
						  data/submissions/Test_data_evaluation.csv\
						  data/prb/anoall.csv\
						  data/submissions/all_predictions.csv\
						  code/run_r_script.sh\
						  code/perform_bootstraps_submissions.R                             
	code/run_r_script.sh code/perform_bootstraps_submissions.R
	
results/Bayes_factor_rmse_violin.pdf: code/run_r_script.sh \
	            data/submissions/Job-393694313420778661233189284.csv \
	            code/run_r_script.sh\
	            results/Bootstrap_RMSE_Results.csv \
						  code/calculate_bayes_factor_plot_rmse.R                             
	code/run_r_script.sh code/calculate_bayes_factor_plot_rmse.R
	

results/eGA_acceleration_table.csv results/eGA_acceleration_clocks.pdf: \
	            data/processed/ano_all_predictions.csv\
			    data/submissions/Test_data_evaluation.csv\
			    code/run_r_script.sh\
			    code/plot_ega_acceleration.R
	code/run_r_script.sh code/plot_ega_acceleration.R


results/tablex.docx results/tabley.docx: exploratory/extract_leaderboard_final_phase_stats.Rmd\
	        data/submissions/Job-393694313420778661233189284.csv\
	        data/submissions/all_predictions.csv\
			    data/prb/anoall.csv\
			    code/run_r_script.sh\
			    data/submissions/Test_data_evaluation.csv\
			    code/render_markdown.R
	code/run_r_script.sh code/render_markdown.R $<


results/plots_mayne.pdf: data/dream_challenge/normalized_450k_combined.Rdata\
	        data/clocks/mayne_clock_coefficients.csv\
	        code/run_r_script.sh\
			    code/plot_parity_eGA_acceleration_mayne_450k.R
	code/run_r_script.sh code/plot_parity_eGA_acceleration_mayne_450k.R 

results/pca_sc1_leaderboard_test.pdf:  data/dream_challenge/Leaderboard_beta_subchallenge1.csv\
          data/dream_challenge/Beta_raw_subchallenge1.csv\
          data/prb/beta_raw.Rdata\
          data/dream_challenge/Sample_annotation_metadata.csv exploratory\
	        data/submissions/Job-393694313420778661233189284.csv\
	        data/submissions/all_predictions.csv\
			    data/prb/anoall.csv\
			    code/run_r_script.sh\
			    code/plot_pca_challenge_data.R
	code/run_r_script.sh code/plot_pca_challenge_data.R

results/heatmap_sc2.pdf:  data/dream_challenge/Beta_normalized_subchallenge2.csv \
        data/dream_challenge/Sample_annotation_metadata.csv \
        data/dream_challenge/Probe_array.csv \
			  code/run_r_script.sh\
			  code/plot_heatmap_challenge_data.R
	code/run_r_script.sh code/plot_heatmap_challenge_data.R
	
results/plot_association_ega_acceleration_environmental_exposures.pdf: \
          data/processed/ano_all_predictions.csv\
	        data/prb/sample_metadata.csv\
	        data/submissions/Test_data_evaluation.csv\
	        code/run_r_script.sh\
			    code/association_ega_acceleration_envrionmental_exposures.R
	code/run_r_script.sh code/association_ega_acceleration_envrionmental_exposures.R 


