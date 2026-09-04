library(igraph)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) stop("Errore: Passa il percorso del file .conf\n")

file_conf <- args[1]
if (!file.exists(file_conf)) stop("Errore: File .conf inesistente\n")

righe_conf <- readLines(file_conf)
riga_progetto <- righe_conf[grep("^project\\s*:", righe_conf)]
if (length(riga_progetto) == 0) stop("Direttiva project: non trovata nel .conf\n")
progetto <- trimws(gsub('project\\s*:\\s*["\']?([^"\']+)["\']?.*', '\\1', riga_progetto))

percorso_base <- paste0("/home/antonio/progetti_analisi/output_", progetto, "/", progetto, "/proximity/")
file_report <- paste0("/home/antonio/progetti_analisi/METRICHE_ESTRATTE_", toupper(progetto), ".txt")

if (!dir.exists(percorso_base)) stop(paste("Errore: Cartella non trovata ->", percorso_base, "\n"))

# Pattern centralizzato per identificare i Bot (Umani + Agenti)
pattern_bot <- "bot|GitHub|actions|copilot|dependabot|renovate|greenkeeper|snyk|sonar|lgtm|bors|homu|mergify|circleci|claude|anthropic|cline|automation|service|travis|appveyor"

estrai_metriche_totale <- function(file_ldot, file_top20) {
  if (!file.exists(file_top20)) {
    return(list(nodi = 0, archi = 0, densita = 0, centralizzazione = 0, nodi_isolati = 0, riga_top_umano = NULL, top_umano_isolato = "NO", id_bot = c(), archi_bot = list(), archi_top_umano = list()))
  }
  
  # 1. Lettura dei nodi ed identificazione preventiva degli ID associati ai Bot nella Top 20
  righe_top20 <- readLines(file_top20)
  nodi_totali <- c()
  id_bot_rilevati <- c()
  
  riga_top_umano <- NULL
  id_top_umano <- NULL
  trovato_top_umano <- FALSE
  
  for (riga in righe_top20) {
    riga_pulita <- gsub("\\s+", " ", trimws(riga))
    parti <- unlist(strsplit(riga_pulita, " "))
    if (length(parti) > 0) {
      primo_token <- parti[1]
      if (grepl("^\\d+$", primo_token)) {
        nodi_totali <- c(nodi_totali, primo_token)
        
        if (grepl(pattern_bot, riga, ignore.case = TRUE)) {
          id_bot_rilevati <- c(id_bot_rilevati, primo_token)
        } else {
          # È un collaboratore umano. Essendo la lista ordinata per commit decrescenti,
          # il primo umano incontrato è matematicamente il Top Contributor Umano.
          if (!trovato_top_umano && length(parti) >= 4) {
            riga_top_umano <- riga  
            id_top_umano <- primo_token # Salviamo l'ID dell'umano più attivo
            trovato_top_umano <- TRUE
          }
        }
      }
    }
  }
  nodi_totali <- unique(nodi_totali)
  id_bot_rilevati <- unique(id_bot_rilevati)
  
  if (length(nodi_totali) == 0) {
    return(list(nodi = 0, archi = 0, densita = 0, centralizzazione = 0, nodi_isolati = 0, riga_top_umano = NULL, top_umano_isolato = "NO", id_bot = c(), archi_bot = list(), archi_top_umano = list()))
  }
  
  # Creiamo il grafo base con i nodi della Top 20
  g <- make_empty_graph(n = length(nodi_totali), directed = FALSE)
  V(g)$name <- nodi_totali
  
  elenco_archi_bot <- list()
  elenco_archi_top_umano <- list() # <-- NUOVA LISTA PER ARCHI UMANO
  
  # 2. Parsing del file .ldot multi-riga
  if (file.exists(file_ldot)) {
    righe <- readLines(file_ldot)
    mappa_id <- character()
    current_node_idx <- NULL
    
    # Primo passaggio: mappiamo TUTTI gli ID reali del file .ldot (anche fuori dalla Top 20)
    for (riga in righe) {
      riga_t <- trimws(riga)
      if (riga_t == "") next
      if (grepl("^\\d+\\s*\\[", riga_t) && !grepl("->|--", riga_t)) {
        current_node_idx <- gsub("^(\\d+)\\s*\\[.*", "\\1", riga_t)
        next
      }
      if (grepl("name=", riga_t) && !is.null(current_node_idx)) {
        riga_pulita <- gsub('"', '', riga_t)
        id_reale <- gsub(".*name=\\s*([0-9]+).*", "\\1", riga_pulita)
        if (id_reale != "") {
          mappa_id[current_node_idx] <- id_reale
        }
        current_node_idx <- NULL 
        next
      }
    }
    
    # Secondo passaggio: estrazione di tutti gli archi dei Bot e del Top Umano
    for (riga in righe) {
      riga_t <- trimws(riga)
      if (grepl("->|--", riga_t)) {
        stringa_arco <- strsplit(riga_t, "\\[")[[1]][1]
        stringa_arco <- gsub("\\s+", "", stringa_arco)
        parti_arco <- unlist(strsplit(stringa_arco, "->|--"))
        
        if (length(parti_arco) >= 2) {
          idx_da <- parti_arco[1]
          idx_a  <- parti_arco[2]
          
          id_da <- mappa_id[idx_da]
          id_a  <- mappa_id[idx_a]
          
          if (!is.null(id_da) && !is.null(id_a) && !is.na(id_da) && !is.na(id_a)) {
            
            # --- AGGIUNTA: Controllo Archi Top Contributor Umano ---
            if (!is.null(id_top_umano) && (id_da == id_top_umano || id_a == id_top_umano)) {
              elenco_archi_top_umano[[length(elenco_archi_top_umano) + 1]] <- c(id_da, id_a)
            }

            # Se almeno uno dei due nodi è un BOT rilevato nella Top 20, includiamo l'arco
            if (id_da %in% id_bot_rilevati || id_a %in% id_bot_rilevati) {
              elenco_archi_bot[[length(elenco_archi_bot) + 1]] <- c(id_da, id_a)
              if (!(id_da %in% V(g)$name)) { g <- add_vertices(g, 1, name = id_da) }
              if (!(id_a %in% V(g)$name))  { g <- add_vertices(g, 1, name = id_a) }
              g <- add_edges(g, c(id_da, id_a))
            } else {
              # Per gli umani manteniamo la regola restrittiva della Top 20
              if (id_da %in% V(g)$name && id_a %in% V(g)$name) {
                g <- add_edges(g, c(id_da, id_a))
              }
            }
          }
        }
      }
    }
  }
  
  g_simple <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
  
  nodi_top20_presenti <- V(g_simple)$name[V(g_simple)$name %in% nodi_totali]
  nodi_isolati <- sum(degree(g_simple, v = nodi_top20_presenti) == 0)
  
  # === NUOVO CONTROLLO: VERIFICA SE IL TOP CONTRIBUTOR UMANO È ISOLATO ===
  top_umano_isolato <- "NO"
  if (!is.null(id_top_umano)) {
    # Se l'ID dell'umano è presente nel grafo, controlliamo il suo grado
    if (id_top_umano %in% V(g_simple)$name) {
      if (degree(g_simple, v = id_top_umano) == 0) {
        top_umano_isolato <- "SI"
      }
    } else {
      # Se non è nemmeno finito nel grafo strutturato (mancanza totale di archi), è isolato
      top_umano_isolato <- "SI"
    }
  }
  
  cent <- 0
  if (vcount(g_simple) > 2) cent <- centralization.degree(g_simple)$centralization
  
  return(list(
    nodi = length(nodi_totali),
    archi = ecount(g_simple),
    densita = round(edge_density(g_simple), 4),
    centralizzazione = round(cent, 4),
    nodi_isolati = nodi_isolati,
    riga_top_umano = riga_top_umano,
    top_umano_isolato = top_umano_isolato,
    id_bot = id_bot_rilevati,
    archi_bot = elenco_archi_bot,
    archi_top_umano = elenco_archi_top_umano # <-- RITORNO DELLA NUOVA LISTA
  ))
}

# Avviamo il sink in scrittura pulita totale sul file
sink(file_report)
cat("=====================================================================\n")
cat(" DATASET METRICHE REALI E COMPONENTE BOT:", toupper(progetto), "\n")
cat(" INCLUSIONE NODI ISOLATI (UMANI + AGENTI) PER COMMUNITY SMELLS\n")
cat("=====================================================================\n\n")

cartelle_release <- sort(list.dirs(percorso_base, full.names = FALSE, recursive = FALSE))

for (rel in cartelle_release) {
  if (rel %in% c("graphs", "ts", "cluster")) next
  
  file_top20 <- paste0(percorso_base, rel, "/top20.numcommits.txt")
  file_ldot  <- paste0(percorso_base, rel, "/sg_reg_all.ldot")
  
  metriche <- estrai_metriche_totale(file_ldot, file_top20)
  
  cat("RELEASE INTERVALLO:", rel, "\n")
  cat("[METRICHE TOPOLOGICHE RETE]\n")
  cat("Nodi_Totali_Contribuenti:", metriche$nodi, "\n")
  cat("Nodi_Isolati (Senza Interazione):", metriche$nodi_isolati, "\n")
  cat("Archi_Collaborazione_Effettivi:", metriche$archi, "\n")
  cat("Densita_Reale_Rete:", metriche$densita, "\n")
  cat("Centralizzazione_Reale:", metriche$centralizzazione, "\n\n")
  
  # === TABELLA AGGIORNATA: INSERITO IL CAMPO "Isolato_Rete" ===
  cat("[TOP CONTRIBUTOR UMANO NELLA RELEASE]\n")
  if (!is.null(metriche$riga_top_umano)) {
    cat(sprintf("%-25s %-12s %-10s %-16s %-15s\n", "Nome_Sviluppatore", "ID_Codeface", "Commit", "Percentuale_%", "Isolato_Rete"))
    riga_pulita <- gsub("\\s+", " ", trimws(metriche$riga_top_umano))
    parti <- unlist(strsplit(riga_pulita, " "))
    if (length(parti) >= 5) {
      len <- length(parti)
      cat(sprintf("%-25s %-12s %-10s %-16s %-15s\n", 
                  paste(parti[3:(len-3)], collapse=" "), 
                  parti[1], 
                  parti[len-2], 
                  parti[len-1], 
                  metriche$top_umano_isolato))
    }
    
    # === NUOVA SEZIONE: ARCHI DEL TOP CONTRIBUTOR UMANO ===
    if (length(metriche$archi_top_umano) > 0) {
      cat("[ARCHI COLLABORATIVI CON TOP HUMAN DETECTED]\n")
      for (arco in metriche$archi_top_umano) {
        cat(paste0(" -> Trovato arco collaborativo tra TOP_UMANO/ID: ", arco[1], " e ID: ", arco[2], "\n"))
      }
    } else {
      cat("[ARCHI COLLABORATIVI CON TOP HUMAN DETECTED]\n Nessun arco di collaborazione trovato per questo sviluppatore.\n")
    }
    cat("\n")
  } else {
    cat("Nessun contributore umano rilevato (la release potrebbe contenere solo bot).\n\n")
  }
  
  # === TABELLA DEI BOT ===
  cat("[BOT RILEVATI NELLA TOP 20]\n")
  if (file.exists(file_top20)) {
    righe_file <- readLines(file_top20)
    righe_bot <- righe_file[grep(pattern_bot, righe_file, ignore.case = TRUE)]
    if (length(righe_bot) > 0) {
      cat(sprintf("%-25s %-10s %-10s %-10s\n", "Nome_Bot", "ID_Codeface", "Commit", "Percentuale_%"))
      for (riga in righe_bot) {
        riga_pulita <- gsub("\\s+", " ", trimws(riga))
        parti <- unlist(strsplit(riga_pulita, " "))
        if (length(parti) >= 5) {
          len <- length(parti)
          cat(sprintf("%-25s %-10s %-10s %-10s\n", paste(parti[3:(len-3)], collapse=" "), parti[1], parti[len-2], parti[len-1]))
        }
      }
      
      if (length(metriche$archi_bot) > 0) {
        cat("\n[ARCHI COLLABORATIVI CON BOT DETECTED]\n")
        for (arco in metriche$archi_bot) {
          cat(paste0(" -> Trovato arco collaborativo tra BOT/ID: ", arco[1], " e ID: ", arco[2], "\n"))
        }
      }
      
    } else { cat("Nessun bot rilevato nella Top 20.\n") }
  } else { cat("Nessun file top20.numcommits.txt presente (Release vuota o singolo sviluppatore).\n") }
  
  cat("---------------------------------------------------------------------\n\n")
}
sink()

cat("Successo! Analisi strutturata per i Community Smell salvata in:", file_report, "\n")