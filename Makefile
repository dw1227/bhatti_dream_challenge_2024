results/rankings_leaderboard_test.pptx: code/run_r_script.sh \
	            data/submissions/Job-393694313420778661233189284.csv \
							data/submissions/Test_data_evaluation.csv \
							code/run_r_script.sh\
							code/get_ranks_leaderboard_test.R	                               
	code/run_r_script.sh code/get_ranks_leaderboard_test.R 




data/processed/ano_all_predictions.csv: data/prb/beta_complete_test_bmiq.Rdata\
              data/prb/anoall.csv\
              data/submissions/Job-393694313420778661233189284.csv\
              data/submissions/Test_data_evaluation.csv\
						  code/run_r_script.sh\
						  code/get_all_predictions.R                             
	code/run_r_script.sh code/get_all_predictions.R
	
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
