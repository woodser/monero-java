package monero.wallet.model;

import com.fasterxml.jackson.annotation.JsonValue;

/**
 * Enumerates transaction priorities.
 */
public enum MoneroTxPriority {
  DEFAULT,
  UNIMPORTANT,
  NORMAL,
  ELEVATED;
  
  @JsonValue
  @Override
  public String toString() {
    return String.valueOf(ordinal()); // TODO: returning ordinal instead of name for jni serialization
  }
}
