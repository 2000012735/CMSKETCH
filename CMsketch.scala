
import chisel3._
import chisel3.util._

class CMsketch(w:Int , d:Int) extends Module {
  val io = IO(new Bundle {
    val datain = Input(UInt(10.W))
    val datard = Input(UInt(10.W))
    val rden = Input(Bool())
    val wren = Input(Bool())
    val dataout = Output(UInt(10.W))
  })


  def Hash(x: UInt, n: UInt, m: Int): UInt = {
    val k = RegInit(0.U(32.W))
    k := (m.U<<x)
    k%n
  }

  def countgen(max: UInt, en: Bool): UInt = {
    val cnt = RegInit(0.U(max.getWidth.W))
    when(en) {
      cnt := Mux(cnt === max-1.U, 0.U, cnt + 1.U)
    }
    cnt
  }
  val Hashin = Reg(Vec(d, UInt(log2Up(w).W)))
  val Hashrd = Reg(Vec(d, UInt(log2Up(w).W)))
  val counter = Reg(Vec(w * d, UInt(32.W)))
  val enable = Reg(Vec(w * d, Bool()))

  when(io.wren) {
    for (i <- 0 until d) {
      Hashin(i) := Hash(io.datain,w.U,i)
    }
    for (i <- 0 until d) {
      for (j <- 0 until w) {
        enable(i*w+j) := j.U === Hashin(i)
      }
    }
    for (i <- 0 until d) {
      for (j <- 0 until w) {
        counter(i*w+j) := countgen(10000000.U, enable(i*w+j))
      }
    }
  }
  val dataoutpre = Reg(Vec(d+1, UInt(32.W)))
      dataoutpre(d) := 10000000.U(32.W)
  val datamin = Reg(UInt(32.W))

  when(io.rden) {
    for (i <- 0 until d) {
      Hashrd(i) := Hash(io.datard,w.U,i)
    }

    for (i <- 0 until (d)) {
      dataoutpre(i) := counter(Hashrd(i) + w.U * i.U )
    }
    for (i <- 0 until (d)) {
      when(dataoutpre(i) >= dataoutpre(i+1)) {
        datamin := dataoutpre(i+1)
      }
    }
  }
  io.dataout := Mux(io.rden,datamin,0.U)
}

object CMsketch extends App{
  Driver.emitVerilog(new CMsketch(90,5))
}