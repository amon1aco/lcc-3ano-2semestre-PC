import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.*;

enum State {
    MENU,
    LOGIN,
    REGISTER,
    APAGAR,
    ESTATISTICAS,
    GAME,
    PLAY,
    END
}

State state;
ConnectionManager conMan;

// MENU
InputField usernameField;
InputField passwordField;

Boolean readerCreated=false;

Boolean userLoggedIn=false;
Boolean waitingInLobby=false;
Boolean apertouPlay = false;


int  matchStatus;
String resp_estatisticas;


// GAME
Player player1 = new Player(255,0,0);
Player player2 = new Player(0,255,0);   // Trocar no cliente2

Sol sol;
Estrela[] estrelas = new Estrela[250];

ArrayList<Planet> planets = new ArrayList<Planet>();

void setup() {
  size(1280,720);
  state = State.MENU;

  // Porta para conectar ao sv
  int porta = 1234; 
  
  // IP do servidor 
  String serverIP = "127.0.0.1"; 

  try{
      conMan = new ConnectionManager(serverIP, porta);
  }
  catch(Exception e){
  }
    // MENU
    background(color(200));
    
    usernameField = new InputField("Username...");
    passwordField = new InputField("Password...");
    
    // GAME
    // sol
    sol = new Sol(width/2, height/2);

    // estrelas
    for (int i = 0; i < estrelas.length; i++) {
      float x = random(width);
      float y = random(height);
      estrelas[i] = new Estrela(x, y);
    }


}

void draw() {

  switch(state) {
    
    case MENU:
      background(color(230));
      
      // titulo
      fill(0); // Cor do texto
      textSize(32); // Tamanho do texto
      textAlign(CENTER, CENTER); // Alinhamento do texto
      text("Jogo Gravidade", width/2, height/2 - 100);
      
      // botão "Login"
      fill(100); // Cor do botão
      rect(width/2-75, height/2+25, 150, 50, 10); // Retângulo do botão "Login"
      fill(255); // Cor do texto do botão
      text("Login", width/2, height/2 + 50); // Texto do botão "Login"
      
      // botão "Registar"
      fill(100); // Cor do botão
      rect(width/2-75, height/2 + 100, 150, 50, 10); // Retângulo do botão "Registar"
      fill(255); // Cor do texto do botão
      text("Registar", width/2, height/2 + 120); // Texto do botão "Registar"
      
      // botão "Estatisticas"
      fill(100); // Cor do botão
      rect(width/2-75, height/2 + 175, 150, 50, 10); // Retângulo do botão "Estatisticas"
      fill(255); // Cor do texto do botão
      text("Estatísticas", width/2, height/2 + 198); // Texto do botão "Estatisticas"
      
      // botão "Delete Account"
      fill(100); // Cor do botão
      rect(width/2-75, height/2 + 250, 150, 50, 10); // Retângulo do botão "Delete Account"
      fill(255); // Cor do texto do botão
      textSize(26);
      text("Apagar Conta", width/2, height/2 + 273); // Texto do botão "Delete Account"
      
      break;

    case PLAY:
      background(200); 
      if(userLoggedIn){
          fill(0,0,0);
          textSize(24);
          text("User logged in",100,30);
      }
      else if(waitingInLobby){
          fill(0,0,0);
          textSize(24);
          text("User joined the lobby",10,60);
      }
      
      fill(0,0,0);
      textSize(24);
      text("Player: " + usernameField.value, 110, 70); // Adiciona o nome do usuário logado

      if(apertouPlay == true){
        textSize(50);
        text("Esperando Jogadores...", 300, 70);
        textSize(24);
      }
      
      fill(100); // Cor do botão
      rect(width/2 - 60, height/2 + 135, 150, 50, 10); // Retângulo do botão de ação
      fill(255); // Cor do texto do botão
      text("PLAY", width/2 + 15, height/2 + 155); // Texto do botão de ação
      
      fill(100); // Cor do botão
      rect(width/2 - 60, height/2 + 205, 150, 50, 10); // Retângulo do botão de ação
      fill(255); // Cor do texto do botão
      text("Logout", width/2 + 15, height/2 + 225); // Texto do botão de ação
       
      break;

    case LOGIN:
      background(200);     // Fundo
      
      fill(100); // Cor do botão
      rect(width/2 - 60, height/2 + 60, 150, 50, 10); // Retângulo do botão de ação
      fill(255); // Cor do texto do botão
      text("Submit", width/2 + 15, height/2 + 84); // Texto do botão de ação
      
      stroke(0); // Define a cor do traço como preto
      strokeWeight(5); // Define a largura do traço
      line(1180 - 20, 70 - 20, 1180 + 20, 70 + 20); // Linha de canto superior esquerdo para canto inferior direito
      line(1180 + 20, 70 - 20, 1180 - 20, 70 + 20); // Linha de canto superior direito para canto inferior esquerdo
      strokeWeight(1); 
      
      fill(0); // Cor do texto
      textSize(20); // Tamanho do texto
      
      text("Username:", width/2 - 100, height/2 - 50); 

      rect(width/2 - 50, height/2 - 60, 200, 30, 5); // Caixa de entrada para o username
      
      text("Password:", width/2 - 100, height/2); 

      rect(width/2 - 50, height/2 - 10, 200, 30, 5); // Caixa de entrada para a password

      textSize(20);
      fill(255);
      text(usernameField.text, width/2 + 10, height/2 - 50);
      text(passwordField.text, width/2 + 10, height/2);

      break;

    case REGISTER:
      background(200);     // Fundo
      
      fill(100); // Cor do botão
      rect(width/2 - 60, height/2 + 60, 150, 50, 10); // Retângulo do botão de ação
      fill(255); // Cor do texto do botão
      text("Resgistar", width/2 + 15, height/2 + 80); // Texto do botão de ação
      
      stroke(0); // Define a cor do traço como preto
      strokeWeight(5); // Define a largura do traço
      line(1180 - 20, 70 - 20, 1180 + 20, 70 + 20); // Linha de canto superior esquerdo para canto inferior direito
      line(1180 + 20, 70 - 20, 1180 - 20, 70 + 20); // Linha de canto superior direito para canto inferior esquerdo
      strokeWeight(1);
      
      fill(0); // Cor do texto
      textSize(20); // Tamanho do texto
      
      text("Username:", width/2 - 100, height/2 - 50); 
      
      rect(width/2 - 50, height/2 - 60, 200, 30, 5); // Caixa de entrada para o username
      
      text("Password:", width/2 - 100, height/2); 
      rect(width/2 - 50, height/2 - 10, 200, 30, 5); // Caixa de entrada para a password
      
      // INPUTs
      textSize(20);
      fill(255);
      text(usernameField.text, width/2 + 10, height/2 - 50);
      text(passwordField.text, width/2 + 10, height/2);
      break;
      
    case APAGAR:
      background(200);     // Fundo
      
      fill(100); // Cor do botão
      rect(width/2 - 60, height/2 + 60, 150, 50, 10); // Retângulo do botão de ação
      fill(255); // Cor do texto do botão
      text("Delete", width/2 + 15, height/2 + 84); // Texto do botão de ação
      
      stroke(0); // Define a cor do traço como preto
      strokeWeight(5); // Define a largura do traço
      line(1180 - 20, 70 - 20, 1180 + 20, 70 + 20); // Linha de canto superior esquerdo para canto inferior direito
      line(1180 + 20, 70 - 20, 1180 - 20, 70 + 20); // Linha de canto superior direito para canto inferior esquerdo
      strokeWeight(1); 
      
      fill(0); // Cor do texto
      textSize(20); // Tamanho do texto
      
      text("Username:", width/2 - 100, height/2 - 50); 

      rect(width/2 - 50, height/2 - 60, 200, 30, 5); // Caixa de entrada para o username
      
      text("Password:", width/2 - 100, height/2); 

      rect(width/2 - 50, height/2 - 10, 200, 30, 5); // Caixa de entrada para a password

      textSize(20);
      fill(255);
      text(usernameField.text, width/2 + 10, height/2 - 50);
      text(passwordField.text, width/2 + 10, height/2);

      break;
      
    case ESTATISTICAS:
      background(200);     // Fundo
    
      text(formataEstatisticas(resp_estatisticas), width / 2 + 15, height / 2 + 80); // Texto das estatísticas
    
      stroke(0); // Define a cor do traço como preto
      strokeWeight(5); // Define a largura do traço
      line(1180 - 20, 70 - 20, 1180 + 20, 70 + 20); // Linha de canto superior esquerdo para canto inferior direito
      line(1180 + 20, 70 - 20, 1180 - 20, 70 + 20); // Linha de canto superior direito para canto inferior esquerdo
      strokeWeight(1);
    
      fill(0); // Cor do texto
      textSize(20); // Tamanho do texto
    
      text("Estatísticas:", width / 2, height / 2 - 50); 
    
      break;
      
    case GAME:
      background(10);

      sol.display();

      for (int i = 0; i < estrelas.length; i++){
        estrelas[i].desenhar();
      }

      player1.render();
      player2.render();
      
      fill(this.player1.r, this.player1.g, this.player1.b);
      textSize(24);
      fill(255);
      text("combustivel do " + this.player1.name+": " + player1.combustivel, 200,30);
      fill(this.player2.r, this.player2.g, this.player2.b);
      textSize(24);
      fill(255);
      text("combustivel do " + this.player2.name+": " + player2.combustivel,  200, 60);
      fill(255);

      if (readerCreated==false){
          readerCreated=true;
          new Thread( () -> {
            try{              
              String debug = this.conMan.receive();
              while(debug!=null){
                parser(debug);
                debug=this.conMan.receive();
              }
            }
            catch(Exception e){
              e.printStackTrace();}
          }).start();
      }
  
      Iterator<Planet> planetIterator = planets.iterator();

      while (planetIterator.hasNext()) {
          Planet b = planetIterator.next();
          b.render();
      } 
      planetIterator = planets.iterator();

      break;

    case END:

      background(200);     // Fundo
      fill(10);
      textSize(50);

      if (matchStatus == 2){
        text("GANHOU!", 660, 350);
      }
      else if(matchStatus == 1){
        text("PERDEU", 660, 350);
      }
      
      fill(100); // Cor do botão
      rect(width/2 - 110, height/2 + 127, 250, 65, 10); // Retângulo do botão de ação
      fill(255); // Cor do texto do botão
      text("Menu", width/2 + 15, height/2 + 157); // Texto do botão de ação
      
      break;
  }
}

void parser(String string) {
    if (string.charAt(0)=='P') { // [P,pid1,x1,y1,p1,pid2,x2,y2,p2,combustivel]
        String[] players = string.split(";");
        for(String str : players){
        String[] tokens = str.split(",");
          if (tokens[1].equals(this.usernameField.value)) { // [P,pid1,x1,y1,p1,pid2,x2,y2,p2,combustivel]
              player1.updatePlayer(Float.parseFloat(tokens[2]), Float.parseFloat(tokens[3]), Float.parseFloat(tokens[4]), Float.parseFloat(tokens[5]), Integer.parseInt(tokens[6]), tokens[1]);
              //player2.updatePlayer(Float.parseFloat(tokens[6]), Float.parseFloat(tokens[7]), Integer.parseInt(tokens[8]));
          } else {
              //player1.updatePlayer(Float.parseFloat(tokens[6]), Float.parseFloat(tokens[7]), Integer.parseInt(tokens[8]));
              player2.updatePlayer(Float.parseFloat(tokens[2]), Float.parseFloat(tokens[3]), Float.parseFloat(tokens[4]), Float.parseFloat(tokens[5]), Integer.parseInt(tokens[6]),tokens[1]);
          }
        }
    }
 
    else if (string.charAt(0) == 'I') { 
      planets = new ArrayList<Planet>();
      String[] parsed = string.split(";");
      for (String str : parsed){
        String[] tokens = str.split(",");
        float x = Float.parseFloat(tokens[1]);
        float y = Float.parseFloat(tokens[2]);
        float size = Float.parseFloat(tokens[3]);
        planets.add(new Planet(x, y,size));
      }
    }
    else if(string.equals("You won!")){
      //("ganhou playboy");
      this.matchStatus = 2; 
      this.state=State.END;
    }
   else if(string.equals("You lost!")){
      //println("perdeu playboy");
      this.matchStatus = 1; 
      this.state=State.END;
    }
}

void keyPressed() {
    
    if (usernameField.isActive()) {
        //atualiza campo com o input do utilizador
        usernameField.text=usernameField.value;
        usernameField.processKey(key);
    }
    if (passwordField.isActive()) {
        //atualiza campo com o input do utilizador
        passwordField.text=passwordField.value;
        passwordField.processKey(key);
    }
    if(state==State.GAME){
       this.conMan.sendKey(key); 
    }
}

void keyReleased() {
  if(state==State.GAME)
    this.conMan.releaseKey(key);
}

void mouseClicked(){
   print("X: " + mouseX + ", Y: " + mouseY);
  //clicar nos botões
  if(mouseX > 565 && mouseX < 720 && state==State.MENU){ //abcissas
    if(mouseY > 385 && mouseY < 435){ //ordenadas do botão login
      //passar para menu de login
      state=State.LOGIN;   
    }
    //passar para menu de registo
    if(mouseY > 460 && mouseY < 510){ //ordenadas do botão register
      state=State.REGISTER;
    }
    //passar para menu de estatisticas
    if(mouseY > 535 && mouseY < 585){ //ordenadas do botão estatisticas
      this.conMan.pedeEstatisticas();
      resp_estatisticas = this.conMan.receive();
      println(resp_estatisticas);
      state=State.ESTATISTICAS;
    }
    if(mouseY > 610 && mouseY < 660){ //ordenadas do botão Delete Account
      state=State.APAGAR;
    }
  }

  if(mouseX > 590 && mouseX < 790 && (state==State.LOGIN || state==State.REGISTER || state == State.APAGAR)){
    if(mouseY > 300 && mouseY < 330){
      //atualiza o username
      usernameField.activate();
      passwordField.deactivate();
    }
    
    if(mouseY > 350 && mouseY < 380){
      //Atualiza a password
      passwordField.activate();
      usernameField.deactivate();
    }
  }
  else{
    if(state==State.LOGIN){
      passwordField.deactivate();
      usernameField.deactivate();
    }
  }
  
  //verificar se o botão de submit foi clicado, se sim, fazer request e analisar a resposta
  //caso esteja no login e este seja válido transita para o ecrã de play
  //caso esteja no registo e este seja válido transita para o ecrã main
  if(mouseX> 580 && mouseX< 730 && mouseY>420 && mouseY<470){
     //verificar se está no ecrã de login
     if(state==State.LOGIN){
        //fazer request e analisar a resposta  
        this.conMan.loginUser(usernameField.value, passwordField.value);
        //println("mandei");
        String response = this.conMan.receive();
        //println("recebi isto1111"+response);
        if(response.equals("Logged in with success!")){
              state = State.PLAY;
              userLoggedIn=true;
        }
     }
     if(state==State.REGISTER){
        this.conMan.registerUser(usernameField.value, passwordField.value);
        String response = this.conMan.receive();
        if(response.equals("User created!")){
              state = State.MENU;
              usernameField.reset();
              passwordField.reset();
        }
     }
     if(state==State.APAGAR){
        this.conMan.apagaConta(usernameField.value, passwordField.value);
        String response = this.conMan.receive();
        if(response.equals("Account deleted with success!")){
              state = State.MENU;
              usernameField.reset();
              passwordField.reset();
        }
        else {
          println(response);
        }
     }
  }
  
if (state == State.PLAY) {
    if (mouseX > 580 && mouseX < 730 && mouseY > 495 && mouseY < 545) {
        apertouPlay = true;
        //textSize(50);
        //fill(0); // Cor do texto do botão
        //text("Esperando por Jogadores...", 320, 350);
      
        if (!waitingInLobby) {
            this.conMan.joinMatch(usernameField.value);
            textSize(50);
            fill(0); // Cor do texto do botão
            text("Esperando por Jogadores...", 320, 350);
            String response = this.conMan.receive();
            //println("recebi isto " + response);
            if (response.equals("User joined the lobby!")) {
                waitingInLobby = true; 
                fill(0, 255, 0);
                textSize(24);
                text("User joined the lobby", 10, 60);
            }
        }

        // Verificar se o segundo botão foi clicado
        if (waitingInLobby) {
            String response = this.conMan.receive();
            //println("2recebi isto2 " + response);
            if (response != null && response.equals("Start")) {
                // Verificar se o botão nas coordenadas especificadas foi clicado
                state = State.GAME;
                userLoggedIn = false;
            }
        }
    }
    if (mouseX > 580 && mouseX < 730 && mouseY > 565 && mouseY < 615) {
      // CODIGO PARA O LOGOUT
      //fazer request e analisar a resposta  
        this.conMan.logoutUser(usernameField.value, passwordField.value);
        //println("mandei");
        String response = this.conMan.receive();
        //println("recebi isto1111"+response);
        if(response != null && response.equals("Logged out with success!")){
              state = State.MENU;
              println("Logout");
              userLoggedIn=false;
        } else {
          println(response);
          state = State.MENU;
        }
    }
}

  
  if (state == State.END) {
   if(mouseX > 530 && mouseX < 780) {
        if(mouseY > 487 && mouseY < 552) {
          state = State.MENU;              
        }
    }
  }
  if(mouseX > 1155 && mouseX< 1205 && mouseY > 45 && mouseY < 95){
    if (state==State.MENU){
       exit();
    }
    else{
      state=State.MENU;
      usernameField.reset();
      passwordField.reset();
    }
  }
}


void mouseMoved() {
    if (state == State.MENU) {
        //atualiza o botão para hover
        // passar o cursor por cima dos botões
        if(mouseX > 565 && mouseX < 720) { //abcissas
            if(mouseY > 385 && mouseY < 435) { //ordenadas do botão login
                cursor(HAND);
            }
            if(mouseY > 460 && mouseY < 510) { //ordenadas do botão register
                cursor(HAND);
            }
            if(mouseY > 535 && mouseY < 585) { //ordenadas do botão estatisticas
                cursor(HAND);
            }
            if(mouseY > 610 && mouseY < 660){ //ordenadas do botão Delete Account
              cursor(HAND);
            }
        } else {
            cursor(ARROW);
        }
    }

    if(mouseX > 1155 && mouseX< 1205 && mouseY > 45 && mouseY < 95){
        cursor(HAND);
    } else {
        if(mouseX > 580 && mouseX < 730) {
            if(mouseY > 420 && mouseY < 470) {
                cursor(HAND);
            }
        } else {
            cursor(ARROW);
        }
    }
    if (state==State.PLAY){
      if (mouseX > 580 && mouseX < 730 && mouseY > 495 && mouseY < 545) {
        cursor(HAND);
      }
      if (mouseX > 580 && mouseX < 730 && mouseY > 565 && mouseY < 615) {
        cursor(HAND);
      }
      else {
        cursor(ARROW);
      }
      
    }
    if (state == State.END) {
      if(mouseX > 530 && mouseX < 780) {
        if(mouseY > 487 && mouseY < 552) {
          cursor(HAND);
        }
        else {
          cursor(ARROW);
        }
      }
    }
}

public static String formataEstatisticas(String stats) {
        // Divide a string a partir de "USERNAME:" e mantém o delimitador "USERNAME:"
        String[] lines = stats.split("(?=USERNAME:)");

        // Junta as linhas com quebras de linha
        StringBuilder formattedStats = new StringBuilder();
        for (String line : lines) {
            formattedStats.append(line).append("\n");
        }

        return formattedStats.toString();
}
