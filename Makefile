results/rankings_leaderboard_test.pptx: code/run_r_script.sh \
	            data/submissions/Job-393694313420778661233189284.csv \
							data/submissions/Test_data_evaluation.csv \
							code/run_r_script.sh\
							code/get_ranks_leaderboard_test.R	                               
	code/run_r_script.sh code/get_ranks_leaderboard_test.R 


results/Parity_plots.pdf data/processed/ano_all_predictions.csv:\
              code/run_r_script.sh \
	            data/submissions/Job-393694313420778661233189284.csv \
						  data/submissions/Test_data_evaluation.csv\
						  data/prb/beta_complete_test_bmiq.Rdata\
						  data/prb/anoall.csv\
						  data/submissions/all_predictions.csv\
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
	
	
exploratory/analyze_eGA_acceleration.docx: exploratory/analyze_eGA_acceleration.Rmd\
	        data/processed/ano_all_predictions.csv\
			    data/submissions/Test_data_evaluation.csv\
			    code/run_r_script.sh\
			    code/render_markdown.R
	code/run_r_script.sh code/render_markdown.R $<


results/tablex.docx results/tabley.docx: exploratory/extract_leaderboard_final_phase_stats.Rmd\
	        data/submissions/Job-393694313420778661233189284.csv\
	        data/submissions/all_predictions.csv\
			    data/prb/anoall.csv\
			    code/run_r_script.sh\
			    data/submissions/Test_data_evaluation.csv\
			    code/render_markdown.R
	code/run_r_script.sh code/render_markdown.R $<



