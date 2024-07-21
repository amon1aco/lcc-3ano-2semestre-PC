import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.*;

class ConnectionManager {
    private Socket socket;
    private BufferedReader in;
    private PrintWriter out;
    public Boolean isActive;
    StringBuilder sb = new StringBuilder();

  public ConnectionManager(String host, int port) {
    try {
      this.socket = new Socket(host, port);
      this.in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
      this.out = new PrintWriter(socket.getOutputStream());
      this.isActive = true;
      print("Sucesso\n");
    }
    catch (IOException e) {
      this.isActive = false;
      e.printStackTrace();
    }
  }
 

  public String receive() {
    String message = "";
    try{
    
      message = this.in.readLine();
    }
    catch (IOException e) {
      message = "An error has occured";
    }
    return message;
  }
  
  public Boolean registerUser(String username, String password){
      this.out.println("create_account,"+username+","+password);
      this.out.flush();
      return true;
  }
  
  public Boolean loginUser(String username, String password){
      if(this.isActive){
        this.out.println("login,"+username+","+password);
        this.out.flush();
      }
      //verificar resposta para saver se foic om suceesso
      return true;
  }
  
  public Boolean logoutUser(String username, String password){
    this.out.println("logout,"+username+","+password);
    this.out.flush();
    return true;
  }
  
  public void joinMatch(String username){
    this.out.println("join,"+username);
    this.out.flush();
  }

  public Boolean pedeEstatisticas(){
      this.out.println("statistics, aux");
      this.out.flush();
      return true;
  }
  
  public Boolean apagaConta(String username, String password){
      this.out.println("delete_account,"+username+","+password);
      this.out.flush();
      return true;
  }
  
  public void sendKey(char key){
    if(key=='w' || key=='a' || key=='d'){
       this.out.println("KeyChanged,"+key+",True\n");
       this.out.flush();
    }
  }
  
  public void releaseKey(char key){
    this.out.println("KeyChanged,"+key+",False\n");
    this.out.flush();
  }
}
