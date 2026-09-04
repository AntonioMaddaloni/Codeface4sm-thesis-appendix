import os
import requests
import mysql.connector
from datetime import datetime
import re
import time
from dotenv import load_dotenv

# Carica il file specifico delle variabili d'ambiente
load_dotenv(dotenv_path=".env.script")

# Configurazione Database
DB_CONFIG = {
    "host": os.getenv("DB_HOST"),
    "user": os.getenv("DB_USER"),
    "password": os.getenv("DB_PASSWORD"),
    "database": os.getenv("DB_DATABASE")
}

# ID Progetto per cdc_dbt_codegen
PROJECT_ID = 120

def get_or_create_mailing_list(cursor):
    cursor.execute(
        "SELECT id FROM mailing_list WHERE projectId = %s AND name = 'GitHub Comments'",
        (PROJECT_ID,)
    )
    res = cursor.fetchone()
    if res:
        return res[0]
    
    print(f"Mailing list non trovata per il progetto {PROJECT_ID}. Creazione in corso...")
    cursor.execute(
        "INSERT INTO mailing_list (name, projectId) VALUES ('GitHub Comments', %s)",
        (PROJECT_ID,)
    )
    return cursor.lastrowid

def get_or_create_author(cursor, username):
    # 1. Verifica se esiste già per nome e progetto
    cursor.execute(
        "SELECT id FROM person WHERE name = %s AND projectId = %s", 
        (username, PROJECT_ID)
    )
    res = cursor.fetchone()
    if res:
        return res[0]
    
    # 2. Se non c'è, inserisci con IGNORE per evitare crash da duplicati sull'indice email
    fake_email = f"{username}@github.com"
    cursor.execute(
        "INSERT IGNORE INTO person (name, projectId, email1) VALUES (%s, %s, %s)",
        (username, PROJECT_ID, fake_email)
    )
    
    if cursor.lastrowid:
        return cursor.lastrowid
    
    # 3. Se l'inserimento è stato ignorato, recupera l'ID del record esistente in sicurezza
    cursor.execute(
        "SELECT id FROM person WHERE projectId = %s AND (name = %s OR email1 = %s)", 
        (PROJECT_ID, username, fake_email)
    )
    res = cursor.fetchone()
    if res:
        return res[0]
        
    return 1  # Fallback globale di sicurezza

def extract_issue_id(issue_url):
    match = re.search(r'/issues/(\d+)', issue_url)
    if match:
        return int(match.group(1))
    return 1000

def fetch_and_insert():
    print("Download dei commenti reali da GitHub in corso (con paginazione)...")
    
    conn = mysql.connector.connect(**DB_CONFIG)
    cursor = conn.cursor()
    
    ml_id = get_or_create_mailing_list(cursor)
    print(f"Utilizzo mlId = {ml_id} per la Foreign Key.")
    
    token = os.getenv("GITHUB_TOKEN")
    headers = {}
    if token:
        headers["Authorization"] = f"token {token}"
        print("Token GitHub configurato correttamente. Rate-limit esteso attivo.")
    else:
        print("ATTENZIONE: GITHUB_TOKEN non trovato nel file .env.script! Richiesta anonima.")

    page = 1
    count = 0
    
    try:
        while True:
            # CORREZIONE: Aggiunto .format() per rendere dinamica la paginazione
            url = "https://api.github.com/repos/OneKeyHQ/app-monorepo/issues/comments?per_page=100&page={page}".format(page=page)
            response = requests.get(url, headers=headers)
            
            if response.status_code != 200:
                print(f"Errore nel download da GitHub alla pagina {page}: {response.status_code}")
                if response.status_code == 403:
                    print("Probabile blocco per Rate Limit o Token non valido. Contenuto risposta:")
                    print(response.json())
                break

            comments = response.json()
            if not comments:
                print("Nessun altro commento trovato. Download completato.")
                break
                
            print(f"Pagina {page}: trovati {len(comments)} commenti.")

            for c in comments:
                sender = c['user']['login']
                created_at = c['created_at']
                issue_url = c['issue_url']
                issue_id = extract_issue_id(issue_url)

                dt = datetime.strptime(created_at, "%Y-%m-%dT%H:%M:%SZ")
                mysql_date = dt.strftime("%Y-%m-%d %H:%M:%S")

                author_id = get_or_create_author(cursor, sender)

                query = """
                    INSERT INTO mail (projectId, threadId, mlId, author, subject, creationDate) 
                    VALUES (%s, %s, %s, %s, %s, %s)
                """
                cursor.execute(query, (PROJECT_ID, issue_id, ml_id, author_id, f"GitHub Issue #{issue_id}", mysql_date))
                count += 1

            conn.commit()
            
            # CORREZIONE: Controllo di sicurezza per evitare loop infiniti se la pagina ha meno di 100 elementi
            if len(comments) < 100:
                print("Raggiunta l'ultima pagina disponibile. Termino il ciclo.")
                break

            page += 1
            time.sleep(0.3) 
            
        print(f"Inserimento completato! Inseriti in totale {count} messaggi sociali.")
        
    except Exception as e:
        conn.rollback()
        print(f"Errore durante l'inserimento nel database: {e}")
    finally:
        cursor.close()
        conn.close()

if __name__ == "__main__":
    fetch_and_insert()