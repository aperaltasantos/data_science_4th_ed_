library(blastula)

email_a_enviar <- render_email("email_automatico.qmd") |> 
  add_attachment(file="output\figures\map_eu_HE.png",
                 filename= "mapa_analise")

smtp_send(email_a_enviar,
          from = "peralta@outlook.com",
          to = "ensp@outlook.com",
          subject= paste0("Análise de dia ", Sys.Date()),
          credentials=creds_file("templates/email_credenciais"))