class InputField{
 float x;
 float y;
 String text="";
 //default text to store
 String dft="";
 String value="";
 boolean isActive;

 InputField(String text){
   this.text=text;
   this.dft=text;
 }
 
 void updateText(String text){
   this.text=text;
 }
 
 void updatePosition(float x, float y){
   this.x=x;
   this.y=y;
 }
 
 void activate(){
    this.isActive=true; 
 }
 
 void deactivate(){
    this.isActive=false; 
 }
 
  boolean isActive(){
    return this.isActive;
  }
 
 void processKey(char key){
   if(this.isActive ){
     int temp=(int)key;
     if (temp==8)
       deleteChar();
     else{
       if(temp>=32 && temp<127)
         this.value+=key;
     }
     this.text=this.value;
   }
 }
 
 void deleteChar(){
      if (value.length() > 0){        
            value = value.substring(0,value.length()-1);
      }
  }
  
  void reset(){
        this.value = "";
        this.text=this.dft;
 }
  
}
