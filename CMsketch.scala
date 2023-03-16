
import chisel3._
import chisel3.util._
import scala.math
class CMsketch(w:Int , d:Int) extends Module {
  val io = IO(new Bundle {
    val datain = Input(UInt(10.W))
    val datard = Input(UInt(10.W))
    val rden = Input(Bool())
    val wren = Input(Bool())
    val dataout = Output(UInt(10.W))
  })


  def Hash(x: UInt, n: UInt, m: Int): UInt = {
    val k = Wire(UInt(32.W))
    k := (x<<m.U)
    k%n
  }

  def countgen(max: UInt, en: Bool): UInt = {
    val cnt = RegInit(0.U(32.W))
    when(en) {
      cnt := Mux(cnt === max-1.U, 0.U, cnt + 1.U)
    }
    cnt
  }
  val Hashin = Wire(Vec(d, UInt(log2Up(w).W)))
  val Hashrd = Wire(Vec(d, UInt(log2Up(w).W)))
  val counter =Wire(Vec(w * d, UInt(32.W)))
  val enable = Wire(Vec(w * d, Bool()))

  when(io.wren){
  for (i <- 0 until d) {
    for (j <- 0 until w) {
      enable(i * w + j) := j.U === Hashin(i)
    }
  }}.otherwise{
    for (i <- 0 until d) {
      for (j <- 0 until w) {
        enable(i * w + j) := false.B
      }
    }
  }

  for (i <- 0 until d) {
    when(io.wren) {
      Hashin(i) := Hash(io.datain, w.U, i + 1)
    }.otherwise{
      Hashin(i) := w.U
    }
  }


    for (i <- 0 until d) {
      for (j <- 0 until w) {
        counter(i*w+j) := countgen(10000000.U, enable(i*w+j))
      }
    }


  val dataoutpre = Wire(Vec(d+1,UInt(32.W)))
      dataoutpre(d) := 10000000.U(32.W)

  when(io.rden){
    for(i <- 0 until d){
      dataoutpre(i) := counter(i.U*w.U + Hashrd(i))
    }
  }.otherwise{
    for (i <- 0 until d) {
      dataoutpre(i) := 0.U
    }
  }

  val datamin = Reg(UInt(32.W))

  when(io.rden) {
    for (i <- 0 until d) {
      Hashrd(i) := Hash(io.datard,w.U,i+1)
    }
  }.otherwise{
    for (i <- 0 until d) {
      Hashrd(i) := 0.U
    }
  }
//  val indexmin = Wire(UInt(32.W))
//  val small    = Wire(Vec(d*d,Bool()))
//  for (i <- 0 until d){
//    for(j <-0 until d){
//      small(i*d+j) := dataoutpre(i)<=dataoutpre(j)
//    }
//  }
//  val smallx   =Wire(Vec(d,Bool()))
//  for (i <- 0 until(d)){
//    for(j <- 0 until(d)){
//      smallx(i) := smallx(i)&small(i*d+j)
//    }
//  }
//  for (i<- 0 until(d)){
//    when(smallx(i)){
//      indexmin := i.U
//    }
//  }
  when(io.rden){
    for(i<-0 until(d)){
      when (i.U===0.U){
        datamin := dataoutpre(0)
      }.otherwise{
        when(dataoutpre(i)<datamin){
          datamin := dataoutpre(i)
        }
      }
    }
  }.otherwise{
    datamin := 0.U
  }

  io.dataout := Mux(io.rden,datamin,0.U)
}


object CMsketch extends App{
  Driver.emitVerilog(new CMsketch(100,5))
}