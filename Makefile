results/rankings_leaderboard_test.pptx: code/run_r_script.sh \
	            data/submissions/Job-393694313420778661233189284.csv \
							data/submissions/Test_data_evaluation.csv \
							code/get_ranks_leaderboard_test.R	                               
	code/run_r_script.sh code/get_ranks_leaderboard_test.R 


results/Parity_plots.pdf: code/run_r_script.sh \
	            data/submissions/Job-393694313420778661233189284.csv \
						  data/submissions/Test_data_evaluation.csv\
						  data/prb/beta_complete_test_bmiq.Rdata\
						  data/prb/anoall.csv\
						  data/submissions/all_predictions.csv\
						  code/generate_parity_plots_predictions.R                             
	code/run_r_script.sh code/generate_parity_plots_predictions.R


results/Bootstrap_RMSE_Results.csv: code/run_r_script.sh \
	            data/submissions/Job-393694313420778661233189284.csv \
						  data/submissions/Test_data_evaluation.csv\
						  data/prb/anoall.csv\
						  data/submissions/all_predictions.csv\
						  code/perform_bootstraps_submissions.R                             
	code/run_r_script.sh code/perform_bootstraps_submissions.R
	
results/Bayes_factor_rmse_violin.pdf: code/run_r_script.sh \
	            data/submissions/Job-393694313420778661233189284.csv \
	            results/Bootstrap_RMSE_Results.csv \
						  code/calculate_bayes_factor_plot_rmse.R                             
	code/run_r_script.sh code/calculate_bayes_factor_plot_rmse.R
