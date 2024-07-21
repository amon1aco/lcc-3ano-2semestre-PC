class Player {
    //PVector pos = new PVector(100,100); // (x,y)
    float x;
    float y;
    float rad; // raio
    float angle;
    float r;
    float g;
    float b;
    String name = "";
    int estado; // 0 a jogar, 1 perdeu, 2 ganhou

    float combustivel;

    Player(float r, float g, float b) {
        this.rad = 35;
        this.r = r;
        this.g=g;
        this.b=b;
        this.combustivel = 0;
        this.estado = 0;
    }

    void updatePlayer(float x, float y, float angle, float combustivel, int estado,String name) {   
        this.x = x;
        this.y = y;
        this.angle = angle;
        this.name=name;
        this.combustivel = combustivel;
        this.estado = estado;
    }

    void render() {
        if (estado == 0){
            pushMatrix();
            translate(x, y);
            rotate(angle);
            stroke(255);
            fill(this.r, this.g, this.b);
            ellipse(0, 0, rad*2, rad*2);
            strokeWeight(5);  
            line(0, 0, rad, 0);
            strokeWeight(1);  
            popMatrix();
        }
    }

}
