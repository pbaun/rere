package rere.driver

import org.mockito.ArgumentCaptor

trait CaptorSugar {
  def captor[T](cls: Class[T]): ArgumentCaptor[T] = ArgumentCaptor.forClass(cls)
}
