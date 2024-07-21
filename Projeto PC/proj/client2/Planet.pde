class Planet {
    float x;
    float y;
    float r;

    Planet(float x, float y, float size) {
        this.x = x;
        this.y = y;
        this.r = size + 5;
    }

    void render() {
        fill(128, 0, 128);       // purpura
        circle(x, y, r*2);
    }
}
