
source("global.R")

shinyServer(function(input, output, session) {
    
    data = reactive({
        
        if (input$data_input_option == 1) { 
            dt_BloodPressure  
        }
        
        else if (input$data_input_option == 2) {
            dt_Plasma
        }
        
        else if (input$data_input_option == 3) {
            dt_t4
        }
        
        else if (input$data_input_option == 4) {
            
            file_in = input$upload_file
            # Avoid error message while file is not uploaded yet
            if (is.null(input$upload_file)) {
                return(data.frame(x = "Click 'Browse...' to select a datafile button"))
            }
            
            else {
                ext = tools::file_ext(file_in$datapath)
                req(file_in)
                validate(need(ext == "csv", "¡Please upload a CSV file other formats are not supported!"))
                dt_data = data.table::fread(file_in$datapath)
                dt_data
            }
        }
        
    })
    
    
    output$raw_data = DT::renderDataTable({
        
        DT::datatable(data(), 
                      options = list(lengthMenu=list(c(5,15,20), c('5','15','20')), pageLength=10,
                                     initComplete = JS("function(settings, json) {",
                                                       "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
                                                        "}"),
                                     columnDefs=list(list(className='dt-center',targets="_all")), searching = FALSE),
                                     filter = "top",
                                     selection = 'multiple',
                                     style = 'bootstrap',
                                     class = 'cell-border stripe',
                                     rownames = FALSE)
                                                      
    })
    
    output$ols_reg = renderPlot({

        data_for_plot = data()
        x = data_for_plot$reference
        y = data_for_plot %>% dplyr::select(!!input$method_list) %>% pull()
        fit.lr = mcr::mcreg(x,y, method.reg="LinReg", na.rm=TRUE)
        mcr::MCResult.plot(fit.lr, add.legend = FALSE, sub=" ")
        
    })
    
    output$olsw_reg = renderPlot({
        
        data_for_plot = data()
        x = data_for_plot$reference
        y = data_for_plot %>% dplyr::select(!!input$method_list) %>% pull()
        fit.lr = mcr::mcreg(x,y, method.reg="WLinReg", na.rm=TRUE)
        mcr::MCResult.plot(fit.lr, add.legend = FALSE, sub=" ")
        
    })
    
    output$deming_reg = renderPlot({
        
        data_for_plot = data()
        x = data_for_plot$reference
        y = data_for_plot %>% dplyr::select(!!input$method_list) %>% pull()
        fit.lr = mcr::mcreg(x,y, method.reg="Deming", na.rm=TRUE)
        mcr::MCResult.plot(fit.lr, add.legend = FALSE, sub=" ")
        
    })
    
    output$demingw_reg = renderPlot({
        
        data_for_plot = data()
        x = data_for_plot$reference
        y = data_for_plot %>% dplyr::select(!!input$method_list) %>% pull()
        fit.lr = mcr::mcreg(x,y, method.reg="WDeming", na.rm=TRUE)
        mcr::MCResult.plot(fit.lr, add.legend = FALSE, sub=" ")
        
    })
    
    output$PB_reg = renderPlot({
        
        data_for_plot = data()
        x = data_for_plot$reference
        y = data_for_plot %>% dplyr::select(!!input$method_list) %>% pull()
        fit.lr = mcr::mcreg(x,y, method.reg="PaBa", na.rm=TRUE)
        mcr::MCResult.plot(fit.lr, add.legend = FALSE, sub=" ")
        
    })
    
    output$bland_altman = renderPlotly({
        
        data_for_plot = data()
        data_for_plot$dif = ( data_for_plot %>% dplyr::select(!!input$method_list) %>% pull() ) - data_for_plot$reference
        
        y_low = round(mean(data_for_plot$dif, na.rm = TRUE) - sd(data_for_plot$dif) * 2, digits = 2)
        y_up = round(mean(data_for_plot$dif, na.rm = TRUE) + sd(data_for_plot$dif) * 2, digits = 2)
        y_mean =  round(mean(data_for_plot$dif, na.rm = TRUE), digits = 2)
        
        p = ggplot(data_for_plot, aes(x = reference, y = dif)) +
            geom_point(alpha = 0.5) +
            geom_hline(yintercept = y_mean, colour = "blue", size = 0.5) +
            geom_hline(yintercept = y_low, 
                       colour = "red", size = 0.5, linetype="dashed") +
            geom_hline(yintercept = y_up, 
                       colour = "red", size = 0.5, linetype="dashed") +
            theme_bw(base_size = 12) +
            labs(title="Difference Plot", x="Reference Method", y="Test(y) - Reference(x)") 
            # annotate("text", x = (max(dt_BloodPressure$reference) + 5), 
            #          y = (y_low - (y_low * .11)), label = paste0("-2 SD", "\n", y_low), color="red") +
            # annotate("text", x = (max(dt_BloodPressure$reference) + 5), 
            #          y = (y_up + (y_up * .11)), label = paste0("+2 SD", "\n", y_up), color="red") +
            # annotate("text", x = (max(dt_BloodPressure$reference) + 5), 
            #          y = (y_mean + (y_mean * 1.60)), label = paste0("Mean", "\n", y_mean), color="blue")
        
        fig = ggplotly(p)
        
        fig
        
    })
    
    output$lr_summary = renderPrint({
        
        x = dt_Plasma$reference
        y = dt_Plasma$test_1
        fit.lr = mcr::mcreg(x,y, method.reg="PaBa", na.rm=TRUE)
        printSummary(fit.lr)
    })
    
    observe({
        data_loaded = data()
        # Columns for test methods
        column_names = names(data_loaded)
        cols_choice_method = sort(column_names[grepl("test", column_names)])
        # Update depending testing
        updateSelectInput(session, "method_list", choices = cols_choice_method, selected="test_1")

    })
    
    output$app_summary = renderPrint({
        rsconnect::setAccountInfo(name='jasam',
                                  token='3BE3FE71B970ECBCF03126DDDE5A431F',
                                  secret='GHpN4FLFEZYukqMuWytujwX+8HJ+7t/Q0z9Jrm7r')
        data_app = rsconnect::applications("jasam")
        data_app = as.data.table(data_app)
        dt_sub = data_app[name == "multi_method_comparison", c("id", "updated_time")]
        print(dt_sub)
    })
    
    output$all_regressions = renderPlotly({
        data_loaded = data()
        
        methods = c("LinReg", "WLinReg", "Deming", "WDeming", "PaBa")
        
        slopes = c()
        models = c()
        intercepts = c()
        for (method in methods) {
            x = data_loaded$reference
            y = data_loaded %>% dplyr::select(!!input$method_list) %>% pull()
            model = mcr::mcreg(x, y, method.reg=method, mref.name="Reference",
                               mtest.name="Test", na.rm=TRUE)
            slopes = c(slopes, mcr::getCoefficients(model)[2])
            intercepts = c(intercepts, mcr::getCoefficients(model)[1])
            method_aux = switch(method,
                                "LinReg" = "Ordinary Least Squares",
                                "WLinReg" = "Weighted Ordinary Least Squares",
                                "Deming" = "Deming",
                                "WDeming" = "Weighted Deming",
                                "Passing-Bablok")
            models = c(models, method_aux)
        }
        df_for_models = data.frame(slope = slopes,
                                   intercept = intercepts,
                                   model = models)
        
        p = ggplot(data = data_loaded, aes(x = reference, y = y)) + 
            geom_point(colour = "black", fill = "white", shape = 21, stroke = 0.2) +
            geom_abline(data=df_for_models, mapping=aes(slope=slope, intercept=intercept, color=model)) +
            # Identity line
            geom_abline(slope=1, intercept=0, color="red", linetype="3") +
            labs(y=input$method_list) +
            theme_bw(base_size = 12)
        
        fig = ggplotly(p)
        
        fig
    })
    
    output$summary_stats = DT::renderDataTable({
        
        data_loaded = data()
        # calculate models
        methods = c("LinReg", "WLinReg", "Deming", "WDeming", "PaBa")
        
        slopes = c()
        models = c()
        intercepts = c()
        data_points = c()
        confidence_level = c()
        intercept_interval = c()
        slope_interval = c()
        
        for (method in methods) {
            x = data_loaded$reference
            y = data_loaded %>% dplyr::select(!!input$method_list) %>% pull()
            model = mcr::mcreg(x, y, method.reg=method, mref.name="Reference",
                               mtest.name="Test", na.rm=TRUE)
            slopes = c(slopes, round(mcr::getCoefficients(model)[2], digits=2))
            intercepts = c(intercepts, round(mcr::getCoefficients(model)[1], digits = 2))
            method_aux = switch(method,
                                "LinReg" = "Ordinary Least Squaress",
                                "WLinReg" = "Weighted Ordinary Least Squares",
                                "Deming" = "Deming",
                                "WDeming" = "Weighted Deming",
                                "Passing-Bablok")
            models = c(models, method_aux)
            data_points = c(data_points, nrow(model@data))
            cl = paste0( ((1 - model@alpha) * 100 ), "%" )
            confidence_level = c(confidence_level, cl)
            ci_intercept = paste0("(", round(model@para[1,"LCI"], digits=2), " - ", round(model@para[1,"UCI"], digits=2), ")")
            intercept_interval = c(intercept_interval, ci_intercept)
            ci_slope = paste0("(", round(model@para[2,"LCI"], digits=2), " - ", round(model@para[2,"UCI"], digits=2), ")")
            slope_interval = c(slope_interval, ci_slope)
        }
        
        df_for_models = data.frame(slope = slopes,
                                   intercept = intercepts,
                                   regression_method = models,
                                   intercept_interval = intercept_interval,
                                   slope_interval = slope_interval)
        
        df_for_models = df_for_models[, c("regression_method", "intercept",  "intercept_interval", 
                                          "slope", "slope_interval")]
        
        df_for_models = purrr::map_df(df_for_models, as.character)
        
        DT::datatable(df_for_models,
                      options = list(searching = FALSE, paging = FALSE),
                      class = "row-border",
                      rownames = FALSE)
        
    })
    
    output$downloadReport <- downloadHandler(
        
        filename = function() {
            paste(paste("Reference Method",'vs.',"Tests Methods", '@', Sys.Date()), sep = '.', switch(
                input$format, PDF = 'pdf', HTML = 'html'
            ))
        },
        
        content = function(file) {
            
            src = normalizePath("report_analysis.Rmd")
            owd = setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, "report_analysis.Rmd")
            out = rmarkdown::render("report_analysis.Rmd", switch(
                input$format,
                PDF = pdf_document(), HTML = html_document(), Word = word_document()
            ))
            file.rename(out, file)
            
        }
    )
    
    output$summary_ranking = DT::renderDataTable({
        
        data_loaded = data()
        # calculate models
        methods = c("LinReg", "WLinReg", "Deming", "WDeming", "PaBa")
        # Columns for test methods
        column_names = names(data_loaded)
        cols_choice_method = sort(column_names[grepl("test", column_names)])
        
        slopes = c()
        models = c()
        intercepts = c()
        test_methods = c()
        
        for (test_method in cols_choice_method) {
            
            for (method in methods) {
                x = data_loaded$reference
                y = data_loaded %>% dplyr::select(!!test_method) %>% pull()
                model = mcr::mcreg(x, y, method.reg=method, mref.name="Reference",
                                   mtest.name="Test", na.rm=TRUE)
                test_methods = c(test_methods, test_method)
                slopes = c(slopes, round(mcr::getCoefficients(model)[2], digits=2))
                intercepts = c(intercepts, round(mcr::getCoefficients(model)[1], digits=2))
                method_aux = switch(method,
                                    "LinReg" = "Ordinary Least Squares",
                                    "WLinReg" = "Weighted Ordinary Least Squares",
                                    "Deming" = "Deming",
                                    "WDeming" = "Weighted Deming",
                                    "Passing-Bablok")
                models = c(models, method_aux)
            }
        }
        
        df_for_models = data.table(test_method = test_methods,
                                   slope = slopes,
                                   intercept = intercepts,
                                   regression_method = models)
        
        df_for_models$slope_diff = abs(1 - df_for_models$slope)
        df_for_models = df_for_models[order(df_for_models$slope_diff), ]
        df_for_models = df_for_models[, c("test_method", "regression_method", "slope", "intercept")]
        df_for_models = purrr::map_df(df_for_models, as.character)
        DT::datatable(df_for_models,
                      options = list(searching = FALSE, paging = FALSE),
                      class = "row-border",
                      rownames = FALSE)
    })
    
    output$results_test_hypothesis <- renderUI({
        
        data_loaded = data()
        x = data_loaded$reference
        y = data_loaded %>% dplyr::select(!!input$method_list) %>% pull()
        
        # Calc for hypothesis
        test_hypothesis = t.test(x = x, y = y, alternative = "two.sided", 
                                 conf.level = 1 - 0.05, paired = FALSE, var.equal = TRUE)
        
        withMathJax(
            tags$b("Test for Comparability:"),
            br(),
            br(),
            paste0("Reference method ", "\\( n = \\)  ", length(x)),
            br(),
            paste0("\\(", input$method_list, "\\)", " method ", "\\( n = \\) ", length(y)),
            br(),
            paste0("\\(\\bar{x} =\\) ", round(mean(x), 2)),
            br(),
            paste0("\\(\\bar{y} =\\) ", round(mean(y), 2)),
            br(),
            br(),
            tags$b("Confidence interval"),
            tags$em("(two-sided)"),
            br(),
            br(),
            paste0("\\( \\Rightarrow \\)", (1 - 0.05) * 100, "% CI = ", 
                   "[", round(test_hypothesis$conf.int[1], 2), "; ", round(test_hypothesis$conf.int[2], 2), "]"),
            br(),
            br(),
            tags$b("Hypothesis test"),
            br(),
            br(),
            paste0("\\(H_0 : \\mu_1 - \\mu_2 = \\) ", 0, " and \\(H_1 : \\mu_1 - \\mu_2 \\) ", "\\( \\neq \\) ", 0),
            br(),
            paste0("Conclusion : ", ifelse(test_hypothesis$p.value < 0.05, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
            br(),
            br(),
            tags$b("Interpretation"),
            br(),
            paste0("At the ", 0.05 * 100, "% significance level, ", 
                   ifelse(test_hypothesis$p.value < 0.05, "we reject the null hypothesis that the true difference in means is ", 
                          "we do not reject the null hypothesis that the true difference in means is "),
                   test_hypothesis$null.value, " \\((p\\)-value ", 
                   ifelse(test_hypothesis$p.value < 0.001, "< 0.001", paste0("\\(=\\) ", round(test_hypothesis$p.value, 2))), ")", ".")
            
        )
    })
    
    output$n_points = renderText({ 
        data_loaded = data()
        nrow(data_loaded)
    })
    
    output$get_data_upload_basis_helper = renderUI({
        withMathJax(
            tags$img(src="load_data.PNG"),
            br(),
            "It is the first step (loading the data) to carry out the study of the comparison of methods, there are two possibilities:",
            br(),
            br(),
            "1. Use the demo files which contain sample data to test the application, there are 3 possibilities: blood pressure, plasma and T4, these data were obtained from previous in vitro studies, with open data license and correspond with real world tests.",
            br(),
            "2. It is possible to load your own file (comma separated format) and it must contain a column called: “reference” for the standard or gold standard method and at least 1 or more columns with the methods test that will be compared in the following way: test_1 (for method the first method), test_2 (for the second method), always following the pattern test_n (where the n indicates the # of the comparison method), for illustration you can review the demo data option.",
            br(),
            br(),
            "It is possible that the file has data such as ids, specimen or any other product of the study, however, what should always be kept are the columns: reference and test_n.
            Once the file has been loaded, it will be possible to have the data displayed in a table that lists all the study data, it is not mandatory, but it is recommended to have at least 40 samples for the study according to the guide: CLSI, EP09-A3. "
        )
    })
    
    output$get_method_comparison_helper = renderUI({
        withMathJax(
            tags$img(src="method_comparison.PNG"),
            br(),
            "This option is the central axis of the solution, once the data is loaded it is possible to automatically obtain the comparison of the test methods (test_1..test_n) and see for each of them the result of the application it will be possible to filter for each trial test and see its particular results in 4 tabs:",
            br(),
            br(),
            "1. Summary: option that will allow you to dynamically view all regressions (Ordinary Least Squares, Weighted Ordinary Least Squares, Deming, Weighted Deming and Passing-Bablok) at the same time for comparison.",
            br(),
            "2. Graphical evaluation: Each type of linear regression can be observed separately, it also includes the calculation of the correlation for each model.",
            br(),
            "3. Statistics: Table that will present the values calculated by the models in how many sub-statistics such as: intercept, slope and intervals.",
            br(),
            "4. Comparison test: a hypothesis test to compare the value of the means of the reference method vs the test method is carried out to establish whether under this test the methods can be considered interchangeable"
        )
    })
    
    output$get_method_scoring_helper = renderUI({
        withMathJax(
            tags$img(src="method_scoring.PNG"),
            br(),
            "This is the third step in the study, automatically the software through a grid strategy will combine the test methods vs 
            all available regression models (presented in step 2) and will calculate a ranking based on which test and regression method linear 
            presents the best slope. The information will be presented in a table and the records will be presented in an orderly manner from top to bottom"
        )
        
    })
    
    output$get_report_helper = renderUI({
        withMathJax(
            tags$img(src="method_comparison.PNG"),
            br(),
            "Fourth and last step, once the best test method is known under the best linear regression model, an automatically generated 
            report can be downloaded whose document will contain the details, support and conclusion of the analysis of the method comparison. 
            It will be possible to download in PDF or HTML format."
        )
    })
    
    
})