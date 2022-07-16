package elaboration



import chisel3._
import chisel3.internal.InstanceId
import chisel3.experimental.{annotate, ChiselAnnotation, RunFirrtlTransform}
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import firrtl._
import firrtl.annotations.{Annotation, SingleTargetAnnotation}
import firrtl.annotations.{CircuitTarget, ModuleTarget, InstanceTarget, ReferenceTarget, Target}

import tpu._
import arithmetic._
import tpu.util._


/** An annotation that contains some string information */
case class InfoAnnotation(target: Target, info: String) extends SingleTargetAnnotation[Target] {
  def duplicate(newTarget: Target) = this.copy(target = newTarget)
}


/** A transform that reads InfoAnnotations and prints information about them */
class InfoTransform() extends Transform with DependencyAPIMigration {

  override def prerequisites = firrtl.stage.Forms.HighForm

  override def execute(state: CircuitState): CircuitState = {
    println("Starting transform 'IdentityTransform'")

    val annotationsx = state.annotations.flatMap{
      case InfoAnnotation(a: CircuitTarget, info) =>
        println(s"  - Circuit '${a.serialize}' annotated with '$info'")
        None
      case InfoAnnotation(a: ModuleTarget, info) =>
        println(s"  - Module '${a.serialize}' annotated with '$info'")
        None
      case InfoAnnotation(a: InstanceTarget, info) =>
        println(s"  - Instance '${a.serialize}' annotated with '$info'")
        None
      case InfoAnnotation(a: ReferenceTarget, info) =>
        println(s"  - Component '${a.serialize} annotated with '$info''")
        None
      case a =>
        Some(a)
    }

    state.copy(annotations = annotationsx)
  }
}

object InfoAnnotator {
  def info(component: InstanceId, info: String): Unit = {
    annotate(new ChiselAnnotation with RunFirrtlTransform {
      def toFirrtl: Annotation = InfoAnnotation(component.toTarget, info)
      def transformClass = classOf[InfoTransform]
    })
  }
}

object VerilogMain extends App {
  (new ChiselStage).execute(args, Seq(ChiselGeneratorAnnotation(() => new PE(UInt(8.W), UInt(16.W), UInt(32.W), Dataflow.WS))))
    // (new ChiselStage).emitVerilog(new PE(UInt(8.W), UInt(8.W), UInt(16.W), Dataflow.WS))
}


object MeshMain extends App {
   (new ChiselStage).execute(args, Seq(ChiselGeneratorAnnotation(() => new Mesh(UInt(8.W), UInt(16.W), UInt(32.W), 4, 4, Dataflow.WS))))
}

object MeshDelayMain extends App {
   (new ChiselStage).execute(args, Seq(ChiselGeneratorAnnotation(() => new MeshDelay(UInt(8.W), UInt(16.W), UInt(32.W), 4, 4, Dataflow.WS))))
}


object TPUMain extends App {
   (new ChiselStage).execute(args, Seq(ChiselGeneratorAnnotation(() => new TPU(4, 4, 8, 18, 32, 16, 4, Dataflow.WS))))
}



