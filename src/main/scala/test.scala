@main
def Main =
    Graphics.start
    Chris
object Chris:
    val n = 10
    var x = Array.tabulate[Double](n)( i => ( Math.random()*(Graphics.width-200) ).toInt )
    var y = Array.tabulate[Double](n)( i => ( Math.random()*(Graphics.height-200) ).toInt )
    var vx = Array.tabulate[Double](n)( i => if Math.random() > 0.5 then 1 else -1 )
    var vy = Array.tabulate[Double](n)( i => if Math.random() > 0.5 then 1 else -1 )
    var color = Array.tabulate[String](n)( i => s"rgb(${(Math.random()*255).toInt}, ${(Math.random()*255).toInt}, ${(Math.random()*255).toInt})" )
    var w = Array.tabulate[Double](n)( i => ( Math.random()*200 ).toInt )
    var h = w

    val space = InputSpace()
    val up =  space.register(KeyInput("w"))
    val down =  space.register(KeyInput("s"))
    val left =  space.register(KeyInput("a"))
    val right =  space.register(KeyInput("d"))
    


    def loop = 
        Graphics.clear
        for 
            i <- 0 to n-1
        do

            Graphics.setColor (color (i))
            Graphics.fillRectangle(x(i),y(i),w(i),h(i))
            x(i) = x(i) + vx(i)
            y(i) = y(i) + vy(i) 
            if(up.isPressing){
                vy(i) = vy(i) - 0.1
            }
            if(down.isPressing){
                vy(i) = vy(i) + 0.1
            }
            if(left.isPressing){
                vx(i) = vx(i) - 0.1
            }
            if(right.isPressing){
                vx(i) = vx(i) + 0.1
            }
            if y(i) + h(i) > Graphics.height then bounceY(i)
            if x(i) + w(i) >  Graphics.width then bounceX(i)
            if x(i) < 0 then bounceX(i)
            if y(i) < 0 then bounceY(i)

    def bounceX(i:Int) = 
        vx(i) = -vx(i)
    def bounceY(i:Int) =
        vy(i) = -vy(i) 
import scala.scalajs.js
object Graphics:
    
    val document = js.Dynamic.global.document
    val window = js.Dynamic.global.window
    val canvas = document.getElementById("canvas")
    canvas.width = window.innerWidth
    canvas.height = window.innerHeight
    var ctx = canvas.getContext("2d")

    var x = 0
    var y = 0
    def start = js.timers.setInterval(10){
        Chris.loop
    }
    def setColor(red:Int,green:Int,blue:Int):Unit = 
        setColor(s"rgb($red, $green, $blue)")

    def setColor(color:String):Unit = 
        ctx.fillStyle = color

    def clear:Unit = 
        ctx.clearRect(0,0,canvas.width,canvas.height)
    def fillCircle(x:Double,y:Double,radius:Double) = 
        ctx.beginPath()
        ctx.arc(x, y, radius, 0, 2 * Math.PI, false)
        ctx.fill()
    def fillRectangle(x:Double,y:Double,width:Double,height:Double) = 
        ctx.fillRect(x,y,width,height)
    def height = canvas.height.asInstanceOf [Int]
    def width = canvas.width.asInstanceOf [Int]

import scala.collection.mutable.HashMap

trait Input:
  var isPressing:Boolean = false
  var justPressed:Boolean = false
  var justReleased:Boolean = false
  def update() = 
    justPressed = false
    justReleased = false
  
  def ||(in:Input):ORasOneInput = ORasOneInput(this,in)
  def +(in:Input):ORasTwoInput = ORasTwoInput(this,in)
  def &&(in:Input):ANDInput = ANDInput(this,in)
  def clear = 
    justPressed = false
    justReleased = false
    isPressing = false
  
  
import scala.language.implicitConversions

case object Input:
  var movementX = 0.0
  var movementY = 0.0
  implicit def stringToInput(s:String):Input = s match 
    case "leftclick" => MouseInput(s)
    case "middleclick" => MouseInput(s)
    case "rightclick" => MouseInput(s)
    case _ => KeyInput(s)
  
  implicit def stringToButton(s:String):Int = s match 
    case "leftclick" => 1
    case "middleclick" => 2
    case "rightclick" => 3
    case _ => -1
  
  def update = 
    movementX = 0.0
    movementY = 0.0
  
  def apply(in:Input):Input = in

case class InputSpace(var listen:Boolean = true){
  var Inputs:Set[Input] = Set()
  def register[I <: Input](in:I):I = {Inputs += in;in}
  def clear = Inputs.foreach(_.clear)
  def stop = {
    listen = false
    clear
  }
  def start = {
    clear
    listen = true
  }
  def update = if(listen)Inputs.foreach(_.update())
}

case object MouseInput{
  //change to Array?
  val Mice:HashMap[Int,List[MouseInput]] = HashMap()
  try
    Graphics.document.addEventListener("mousedown", (e:js.Dynamic)=> {
      if(Mice.contains(e.which.asInstanceOf[Int])){
        Mice(e.which.asInstanceOf[Int]).foreach(mouseinput => {
          if(!mouseinput.isPressing) mouseinput.justPressed = true
          mouseinput.isPressing = true
        })
      }
    }, false)
    Graphics.document.document.addEventListener("mouseup", (e:js.Dynamic)=> {
        if(Mice.contains(e.which.asInstanceOf[Int]))Mice(e.which.asInstanceOf[Int]).foreach(mouseinput => {
          if(mouseinput.isPressing) mouseinput.justReleased = true
          mouseinput.isPressing = false
        })
    }, false)
    Graphics.document.document.addEventListener("mousemove", (e:js.Dynamic)=> {
        Mice.values.foreach(_.foreach(mouseinput => {
          mouseinput.movementY += e.movementY.asInstanceOf[Double]
          mouseinput.movementX += e.movementX.asInstanceOf[Double]
        }))
    }, false)

  catch
    case t:Throwable => println(t.getStackTrace())
  
}


case class MouseInput(button:Int) extends Input:
  if(MouseInput.Mice.contains(button))MouseInput.Mice(button) = MouseInput.Mice(button) ::: List(this) else MouseInput.Mice += ((button,List(this)))
  var movementX = 0.0
  var movementY = 0.0
  var posX = 0.0
  var posY = 0.0
  override def update() = 
    posX += movementX
    posY += movementY
    movementX = 0.0
    movementY = 0.0
    super.update()

  override def clear =
    movementX = 0.0
    movementY = 0.0
    posX = 0.0
    posY = 0.0
    super.clear

case class DragInput(button:Int) extends Input {
  val in = MouseInput(button)
  var movementX = 0.0
  var movementY = 0.0
  var posX = 0.0
  var posY = 0.0
  override def update() = {
    
    isPressing = in.isPressing
    justReleased = in.justReleased
    justPressed = in.justPressed
    if(justPressed){
      posX = in.posX
      posY = in.posY
    }
    if(isPressing){
      movementX = in.movementX
      movementY = in.movementY
      posX += movementX
      posY += movementY
      movementX = 0.0
      movementY = 0.0
    }
    in.update()
    super.update()
  }
  override def clear = {
    movementX = 0.0
    movementY = 0.0
    posX = 0.0
    posY = 0.0
    super.clear
  }
}

case object KeyInput{
  val Keys:HashMap[String,List[KeyInput]] = HashMap()
  try{
    Graphics.document.addEventListener("keydown", (e:js.Dynamic)=> {
      e.preventDefault()
      if(Keys.contains(e.key.asInstanceOf[String].toUpperCase)){
        Keys(e.key.asInstanceOf[String].toUpperCase).foreach(keyinput => {
          if(!keyinput.isPressing) keyinput.justPressed = true
          keyinput.isPressing = true
        })
      }
    }, false)
    Graphics.document.addEventListener("keyup", (e:js.Dynamic)=> if(Keys.contains(e.key.asInstanceOf[String].toUpperCase)){
      e.preventDefault()
        Keys(e.key.asInstanceOf[String].toUpperCase).foreach(keyinput => {
          keyinput.justReleased = true
          keyinput.isPressing = false
        })
    }, false)
    
  }catch{
    case t:Throwable => println(t.getStackTrace())
  }
}

case class KeyInput(key:String) extends Input {
  if(KeyInput.Keys.contains(key.toUpperCase))KeyInput.Keys(key.toUpperCase) = KeyInput.Keys(key.toUpperCase) ::: List(this) else KeyInput.Keys += ((key.toUpperCase,List(this)))
  
}
case class DoubleInput(in:Input) extends Input {
  var lastPress:Long = -1
  override def update() = {
    super.update()
    val now = System.currentTimeMillis
    if(in.justPressed){
      if(lastPress != -1){
        if(now - lastPress < 200){
          justPressed = true
        }
      }
      lastPress = now
    }
    if(justPressed){
      isPressing = true
    }
    if(isPressing && in.justReleased){
      isPressing = false
      justReleased = true
    }
    in.update()
  }
}
case class ORasOneInput(in1:Input,in2:Input) extends Input {
  override def update() = {
    super.update()
    if(!isPressing)justPressed = in1.justPressed || in2.justPressed
    isPressing = in1.isPressing || in2.isPressing
    if(!isPressing)justReleased = in1.justReleased || in2.justReleased
    in1.update()
    in2.update()
  }
}
case class ORasTwoInput(in1:Input,in2:Input) extends Input {
  override def update() = {
    super.update()
    justPressed = in1.justPressed || in2.justPressed
    isPressing = in1.isPressing || in2.isPressing
    justReleased = in1.justReleased || in2.justReleased
    in1.update()
    in2.update()
  }
}
case class ANDInput(in1:Input,in2:Input) extends Input {
  override def update() = {
    super.update()
    justPressed = (in1.justPressed && in2.isPressing) || (in2.justPressed && in1.isPressing)
    if(isPressing) justReleased = (in1.justReleased ) || (in2.justReleased)
    isPressing = in1.isPressing && in2.isPressing
    in1.update()
    in2.update()
  }
}
