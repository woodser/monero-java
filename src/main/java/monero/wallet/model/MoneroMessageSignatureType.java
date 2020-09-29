package monero.wallet.model;

import com.fasterxml.jackson.annotation.JsonValue;

/**
 * Enumerate message signature types.
 */
public enum MoneroMessageSignatureType {
  SIGN_WITH_SPEND_KEY,
  SIGN_WITH_VIEW_KEY
}
