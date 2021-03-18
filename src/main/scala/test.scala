
@main
def Main = 
    println("hi")
    val i:Int = 0
    for 
        i <- 1 to 10 
        j <- 1 to 10
    do 
        println( (i, j) ) 
    Graphics.start

def loop = 
    Graphics.fillRectangle(10,10,50,50)




object Graphics:
    import scala.scalajs.js
    val document = js.Dynamic.global.document
    val window = js.Dynamic.global.window
    val canvas = document.getElementById("canvas")
    canvas.width = window.innerWidth
    canvas.height = window.innerHeight
    var ctx = canvas.getContext("2d")
    
    var x = 0
    var y = 0
    def start = js.timers.setInterval(10){
        loop
    }
    def setColor(red:Int,green:Int,blue:Int):Unit = 
        setColor(s"rgb($red, $green, $blue)")
    
    def setColor(color:String):Unit = 
        ctx.fillStyle = color
    
    def clear:Unit = 
        ctx.clearRect(0,0,canvas.width,canvas.height)
    def fillCircle(x:Int,y:Int,radius:Int) = 
        ctx.beginPath()
        ctx.arc(x, y, radius, 0, 2 * Math.PI, false)
        ctx.fill()
    def fillRectangle(x:Int,y:Int,width:Int,height:Int) = 
        ctx.fillRect(x,y,width,height)
    