module Generation.EraSpec where

import Cardano.Base.IP (IPv4, IPv6)
import Cardano.Crypto.DSIGN (SignedDSIGN)
import Cardano.Crypto.KES (VerKeyKES)
import Cardano.Crypto.VRF (VerKeyVRF)
import Cardano.Ledger.Allegra.Scripts (Timelock)
import Cardano.Ledger.Allegra.TxAuxData (AllegraTxAuxData)
import Cardano.Ledger.Alonzo.Scripts (AlonzoEraScript (PlutusPurpose), AsIx, CostModels)
import Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData)
import Cardano.Ledger.Alonzo.TxBody (ScriptIntegrityHash)
import Cardano.Ledger.Alonzo.TxOut (AlonzoTxOut)
import Cardano.Ledger.Alonzo.TxWits (Redeemers)
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary
import Cardano.Ledger.Block (Block)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams (DRepVotingThresholds, PoolVotingThresholds)
import Cardano.Ledger.Core hiding (Era)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.DRep (DRep)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Scripts (AccountBalanceInterval, AccountBalanceIntervals)
import Cardano.Ledger.Keys
import Cardano.Ledger.Mary.Value (AssetName, MaryValue (..), MultiAsset, PolicyID)
import Cardano.Ledger.Metadata (Metadatum)
import Cardano.Ledger.Plutus.Data (BinaryData, Data, Datum, getPlutusData)
import Cardano.Ledger.Plutus.ExUnits (ExUnits, Prices)
import Cardano.Ledger.Plutus.Language (Language, PlutusBinary)
import Cardano.Ledger.Shelley.TxAuxData (ShelleyTxAuxData)
import Cardano.Ledger.State (PoolMetadata, StakePoolRelay)
import Cardano.Ledger.TxIn (TxId, TxIn)
import Cardano.Protocol.Crypto (KES, StandardCrypto, VRF)
import Cardano.Protocol.Leios.BlockHeader qualified as Leios
import Cardano.Protocol.Praos.BlockHeader (Header, HeaderBody)
import Cardano.Protocol.TPraos.OCert (KESPeriod, OCert)
import Coverage.Obligations.SpecRule (HuddleSpec, eraRoot)
import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.ByteString qualified as BS
import Data.OSet.Strict (OSet)
import Data.Sequence.Strict (StrictSeq)
import Data.Text qualified as T
import Generation.Rule
import PlutusLedgerApi.V1 qualified as PV1
import Test.Cardano.Ledger.Conway.Binary.Annotator ()
import Test.Cardano.Ledger.Core.Binary.Annotator ()
import Test.Cardano.Ledger.Dijkstra.Binary.Annotator ()
import Test.Cardano.Protocol.Binary.Annotator ()

-- | An EraSpec specifies a list of rules to be checked for conformance. The protocol version is the highest protocol
--   version that is supported for the associated Era.
data EraSpec = EraSpec
  { eraSpecEra :: Era,
    eraSpecProtocolVersion :: Version,
    eraSpecRules :: [GenerationRule]
  }
  deriving (Show, Eq)

-- | Supported Eras
data Era
  = Conway
  | Dijkstra
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | The name of an era in lower case, as it names the era directories in the datasets directory.
renderEra :: Era -> Text
renderEra = T.toLower . show

instance ToJSON Era where
  toJSON = toJSON . renderEra

instance FromJSON Era where
  parseJSON = withText "era" $ \name ->
    maybe (fail ("unknown era: " <> toString name)) pure (find ((== name) . renderEra) [minBound ..])

eraSpecs :: [EraSpec]
eraSpecs =
  [ EraSpec Conway (eraProtVerHigh @ConwayEra) conwayRules,
    EraSpec Dijkstra (eraProtVerHigh @DijkstraEra) dijkstraRules
  ]

-- | All the Conway rules
conwayRules :: [GenerationRule]
conwayRules =
  let version = eraProtVerHigh @ConwayEra
   in [ mkAnnotatorRule @(Block (Header StandardCrypto) ConwayEra) version "block",
        mkByteExactAnnotatorRule @(Header StandardCrypto) version "header",
        mkRule @(HeaderBody StandardCrypto) version "header_body",
        mkRule @(Tx TopTx ConwayEra) version "transaction",
        mkByteExactRule @(TxBody TopTx ConwayEra) version "transaction_body",
        mkRule @TxIn version "transaction_input",
        mkByteExactRule @(TxWits ConwayEra) version "transaction_witness_set",
        mkRule @(TxOut ConwayEra) version "transaction_output",
        mkRule @(Value ConwayEra) version "value",
        mkRule @(Script ConwayEra) version "script",
        mkRule @(Datum ConwayEra) version "datum_option",
        mkRule @(TxCert ConwayEra) version "certificate",
        mkRule @(OSet (TxCert ConwayEra)) version "certificates",
        mkByteExactRule @(Timelock ConwayEra) version "native_script",
        mkByteExactRule @(Data ConwayEra) version "plutus_data",
        mkRule @ConwaySingleRedeemer version "redeemer",
        mkByteExactRule @(Redeemers ConwayEra) version "redeemers",
        mkByteExactRule @(TxAuxData ConwayEra) version "auxiliary_data",
        mkRule @(Credential Staking) version "credential",
        mkRule @DRep version "drep",
        mkRule @StakePoolRelay version "relay",
        mkRule @(GovAction ConwayEra) version "gov_action",
        mkRule @(VotingProcedure ConwayEra) version "voting_procedure",
        mkRule @(ProposalProcedure ConwayEra) version "proposal_procedure",
        mkRule @(OSet (ProposalProcedure ConwayEra)) version "proposal_procedures",
        mkRule @(PParamsUpdate ConwayEra) version "protocol_param_update",
        mkRule @CostModels version "cost_models",
        mkRule @Voter version "voter",
        mkRule @Metadatum version "metadatum",
        mkRule @DRepVotingThresholds version "drep_voting_thresholds",
        mkRule @PoolVotingThresholds version "pool_voting_thresholds",
        mkRule @ExUnits version "ex_units",
        mkRule @Anchor version "anchor",
        mkRule @GovActionId version "gov_action_id",
        mkRule @(Constitution ConwayEra) version "constitution",
        mkRule @(OCert StandardCrypto) version "operational_cert",
        mkRule @BootstrapWitness version "bootstrap_witness",
        mkRule @(WitVKey Witness) version "vkeywitness",
        mkRule @Withdrawals version "withdrawals",
        mkRule @MultiAsset version "mint",
        mkRule @Addr version "address",
        mkRule @AccountAddress version "reward_account",
        mkRule @ProtVer version "protocol_version",
        mkRule @Coin version "coin",
        mkRule @EpochNo version "epoch",
        mkRule @SlotNo version "slot",
        mkRule @BlockNo version "block_number",
        mkRule @Network version "network_id",
        mkRule @Url version "url",
        mkRule @DnsName version "dns_name",
        mkRule @UnitInterval version "unit_interval",
        mkRule @NonNegativeInterval version "nonnegative_interval",
        mkRule @TxId version "transaction_id",
        mkRule @ScriptHash version "script_hash",
        mkRule @(KeyHash Payment) version "addr_keyhash",
        mkRule @(KeyHash StakePool) version "pool_keyhash",
        mkRule @(VRFVerKeyHash StakePoolVRF) version "vrf_keyhash",
        mkRule @TxAuxDataHash version "auxiliary_data_hash",
        mkRule @PolicyID version "policy_id",
        mkRule @AssetName version "asset_name",
        mkRule @(Credential Staking) version "stake_credential",
        mkRule @(Credential DRepRole) version "drep_credential",
        mkRule @(Credential ColdCommitteeRole) version "committee_cold_credential",
        mkRule @(Credential HotCommitteeRole) version "committee_hot_credential",
        mkRule @Vote version "vote",
        mkRule @Prices version "ex_unit_prices",
        mkRule @PoolMetadata version "pool_metadata",
        mkRule @KESPeriod version "kes_period",
        mkRule @(VKey Witness) version "vkey",
        mkPositiveMultiAssetRule version "%multiasset<positive_coin>",
        -- The ledger reads every output with the Babbage decoder. Its branch for an array is the same as this decoder,
        -- with the same address decoder, and this decoder also refuses the map form, as the rule states.
        mkRule @(AlonzoTxOut ConwayEra) version "alonzo_transaction_output",
        mkRule @Language version "language",
        mkRule @(AllegraTxAuxData ConwayEra) version "auxiliary_data_array",
        mkRule @(StrictSeq (Timelock ConwayEra)) version "auxiliary_scripts",
        mkRule @ScriptHash version "hash28",
        mkRule @TxId version "hash32",
        mkRule @ScriptIntegrityHash version "script_data_hash",
        mkRule @ScriptHash version "guardrails_script_hash",
        mkRule @Version version "major_protocol_version",
        mkRule @(NonZero Coin) version "positive_coin",
        -- The ledger reads a mint amount with a decoder that is local to decodeMultiAsset. That decoder accepts
        -- only a CBOR integer from -2^63 to 2^63-1, and refuses zero. NonZero Int accepts the same values.
        mkRule @(NonZero Int) version "nonzero_int64",
        mkRule @(PlutusInteger ConwayEra) version "big_int",
        mkRule @(PlutusBytes ConwayEra) version "bounded_bytes",
        mkRule @IPv4 version "ipv4",
        mkRule @IPv6 version "ipv6",
        mkRule @(VerKeyKES (KES StandardCrypto)) version "kes_vkey",
        mkRule @(VerKeyVRF (VRF StandardCrypto)) version "vrf_vkey",
        mkRule @(SignedDSIGN DSIGN (Hash HASH EraIndependentTxBody)) version "signature",
        -- signkey_kes has no rule: no other rule refers to it, a KES signing key is never on chain, and no decoder
        -- reads it. Its size, 64 bytes, is the size of an Ed25519 signature, not of a Sum6KES signing key.
        mkRule @(Data ConwayEra) version "%constr<plutus_data>",
        mkRule @PlutusBinary version "plutus_v1_script",
        mkRule @PlutusBinary version "plutus_v2_script",
        mkRule @PlutusBinary version "plutus_v3_script",
        mkRule @(BinaryData ConwayEra) version "data",
        mkRule @(ShelleyTxAuxData ConwayEra) version "metadata",
        mkRule @(AlonzoTxAuxData ConwayEra) version "auxiliary_data_map",
        mkRule @(VotingProcedures ConwayEra) version "voting_procedures",
        mkRule @(BabbageTxOut ConwayEra) version "babbage_transaction_output"
      ]

-- | All the Dijkstra rules
dijkstraRules :: [GenerationRule]
dijkstraRules =
  let version = eraProtVerHigh @DijkstraEra
   in [ -- A Dijkstra block carries the Leios header, which adds the Leios certificate flag and the endorser block
        -- announcement to the Praos header body.
        mkAnnotatorRule @(Block (Leios.Header StandardCrypto) DijkstraEra) version "block",
        mkByteExactAnnotatorRule @(Leios.Header StandardCrypto) version "header",
        mkRule @(Leios.HeaderBody StandardCrypto) version "header_body",
        mkRule @Leios.EbAnnouncement version "eb_announcement",
        mkRule @(BlockBody DijkstraEra) version "block_body",
        mkRule @(Tx TopTx DijkstraEra) version "transaction",
        mkByteExactRule @(TxBody TopTx DijkstraEra) version "transaction_body",
        mkRule @(TxBody SubTx DijkstraEra) version "sub_transaction_body",
        mkRule @TxIn version "transaction_input",
        mkByteExactRule @(TxWits DijkstraEra) version "transaction_witness_set",
        mkRule @(TxOut DijkstraEra) version "transaction_output",
        mkRule @(Value DijkstraEra) version "value",
        mkRule @(Script DijkstraEra) version "script",
        mkRule @(Datum DijkstraEra) version "datum_option",
        mkRule @(TxCert DijkstraEra) version "certificate",
        mkRule @(OSet (TxCert DijkstraEra)) version "certificates",
        mkByteExactRule @(NativeScript DijkstraEra) version "native_script",
        mkByteExactRule @(Data DijkstraEra) version "plutus_data",
        mkByteExactRule @(Redeemers DijkstraEra) version "redeemers",
        mkByteExactRule @(TxAuxData DijkstraEra) version "auxiliary_data",
        mkRule @(Credential Staking) version "credential",
        mkRule @DRep version "drep",
        mkRule @StakePoolRelay version "relay",
        mkRule @(GovAction DijkstraEra) version "gov_action",
        mkRule @(VotingProcedure DijkstraEra) version "voting_procedure",
        mkRule @(VotingProcedures DijkstraEra) version "voting_procedures",
        mkRule @(ProposalProcedure DijkstraEra) version "proposal_procedure",
        mkRule @(OSet (ProposalProcedure DijkstraEra)) version "proposal_procedures",
        mkRule @(PParamsUpdate DijkstraEra) version "protocol_param_update",
        mkRule @CostModels version "cost_models",
        mkRule @(AccountBalanceInterval DijkstraEra) version "account_balance_interval",
        mkRule @(AccountBalanceIntervals DijkstraEra) version "account_balance_intervals",
        mkRule @Voter version "voter",
        mkRule @Metadatum version "metadatum",
        mkRule @DRepVotingThresholds version "drep_voting_thresholds",
        mkRule @PoolVotingThresholds version "pool_voting_thresholds",
        mkRule @ExUnits version "ex_units",
        mkRule @Anchor version "anchor",
        mkRule @GovActionId version "gov_action_id",
        mkRule @(Constitution DijkstraEra) version "constitution",
        mkRule @(OCert StandardCrypto) version "operational_cert",
        mkRule @BootstrapWitness version "bootstrap_witness",
        mkRule @(WitVKey Witness) version "vkeywitness",
        mkRule @Withdrawals version "withdrawals",
        mkRule @MultiAsset version "mint",
        mkRule @Addr version "address",
        mkRule @AccountAddress version "reward_account",
        mkRule @ProtVer version "protocol_version",
        mkRule @Coin version "coin",
        mkRule @EpochNo version "epoch",
        mkRule @SlotNo version "slot",
        mkRule @BlockNo version "block_number",
        mkRule @Network version "network_id",
        mkRule @Url version "url",
        mkRule @DnsName version "dns_name",
        mkRule @UnitInterval version "unit_interval",
        mkRule @NonNegativeInterval version "nonnegative_interval",
        mkRule @TxId version "transaction_id",
        mkRule @ScriptHash version "script_hash",
        mkRule @(KeyHash Payment) version "addr_keyhash",
        mkRule @(KeyHash StakePool) version "pool_keyhash",
        mkRule @(VRFVerKeyHash StakePoolVRF) version "vrf_keyhash",
        mkRule @TxAuxDataHash version "auxiliary_data_hash",
        mkRule @PolicyID version "policy_id",
        mkRule @AssetName version "asset_name",
        mkRule @(Credential Staking) version "stake_credential",
        mkRule @(Credential DRepRole) version "drep_credential",
        mkRule @(Credential ColdCommitteeRole) version "committee_cold_credential",
        mkRule @(Credential HotCommitteeRole) version "committee_hot_credential",
        mkRule @Vote version "vote",
        mkRule @Prices version "ex_unit_prices",
        mkRule @PoolMetadata version "pool_metadata",
        mkRule @KESPeriod version "kes_period",
        mkRule @(VKey Witness) version "vkey",
        mkPositiveMultiAssetRule version "%multiasset<positive_coin>",
        -- The ledger reads every output with the Babbage decoder. Its branch for an array is the same as this decoder,
        -- with the same address decoder, and this decoder also refuses the map form, as the rule states.
        mkRule @(AlonzoTxOut DijkstraEra) version "alonzo_transaction_output",
        mkRule @Language version "language",
        mkRule @(AllegraTxAuxData DijkstraEra) version "auxiliary_data_array",
        mkRule @(StrictSeq (Timelock DijkstraEra)) version "auxiliary_scripts",
        mkRule @ScriptHash version "hash28",
        mkRule @TxId version "hash32",
        mkRule @ScriptIntegrityHash version "script_data_hash",
        mkRule @ScriptHash version "guardrails_script_hash",
        mkRule @Version version "major_protocol_version",
        mkRule @(NonZero Coin) version "positive_coin",
        -- The ledger reads a mint amount with a decoder that is local to decodeMultiAsset. That decoder accepts
        -- only a CBOR integer from -2^63 to 2^63-1, and refuses zero. NonZero Int accepts the same values.
        mkRule @(NonZero Int) version "nonzero_int64",
        mkRule @(PlutusInteger DijkstraEra) version "big_int",
        mkRule @(PlutusBytes DijkstraEra) version "bounded_bytes",
        mkRule @IPv4 version "ipv4",
        mkRule @IPv6 version "ipv6",
        mkRule @(VerKeyKES (KES StandardCrypto)) version "kes_vkey",
        mkRule @(VerKeyVRF (VRF StandardCrypto)) version "vrf_vkey",
        mkRule @(SignedDSIGN DSIGN (Hash HASH EraIndependentTxBody)) version "signature",
        -- signkey_kes has no rule: no other rule refers to it, a KES signing key is never on chain, and no decoder
        -- reads it. Its size, 64 bytes, is the size of an Ed25519 signature, not of a Sum6KES signing key.
        mkRule @(Data DijkstraEra) version "%constr<plutus_data>",
        mkRule @PlutusBinary version "plutus_v1_script",
        mkRule @PlutusBinary version "plutus_v2_script",
        mkRule @PlutusBinary version "plutus_v3_script",
        mkRule @(BinaryData DijkstraEra) version "data",
        mkRule @(ShelleyTxAuxData DijkstraEra) version "metadata",
        mkRule @(AlonzoTxAuxData DijkstraEra) version "auxiliary_data_map",
        mkRule @(BabbageTxOut DijkstraEra) version "babbage_transaction_output"
      ]

-- | The name an era is used to name era directories in the datasets directory
eraSpecName :: EraSpec -> Text
eraSpecName = renderEra . eraSpecEra

eraSpecRoot :: EraSpec -> Either Text HuddleSpec
eraSpecRoot = eraRoot . eraSpecName

-- | Return an EraSpec given its name
lookupEra :: Text -> Either Text EraSpec
lookupEra = lookupNamed "era" eraSpecName eraSpecs

ruleNames :: EraSpec -> [RuleName]
ruleNames = map ruleName . eraSpecRules

lookupRule :: EraSpec -> RuleName -> Either Text GenerationRule
lookupRule era =
  lookupNamed (eraSpecName era <> " rule") (unRuleName . ruleName) (eraSpecRules era) . unRuleName

lookupNamed :: Text -> (a -> Text) -> [a] -> Text -> Either Text a
lookupNamed description getName values requested =
  case find ((== requested) . getName) values of
    Just value -> Right value
    Nothing ->
      Left $
        "unknown "
          <> description
          <> " '"
          <> requested
          <> "': expected "
          <> T.intercalate ", " (map getName values)

-- | Get the era rules, possibly restricted to a subset
getRulesToGenerate :: EraSpec -> [RuleName] -> [GenerationRule]
getRulesToGenerate era onlyRules = case onlyRules of
  [] -> eraSpecRules era
  names -> filter ((`elem` names) . ruleName) (eraSpecRules era)

-- | One redeemer as the @redeemer@ rule of the Conway specification states it: @[tag, index, data, ex_units]@.
--
-- The ledger has no type for one redeemer on its own: it decodes a redeemer only as an element of 'Redeemers', which
-- also accepts the map form. This type gives the rule something to decode by itself, the same way the ledger decodes
-- one element of the array form. The tag and the index make up one 'PlutusPurpose' group, which is why the cbor array
-- has 4 fields and the data type only 3.
data ConwaySingleRedeemer
  = ConwaySingleRedeemer
      (PlutusPurpose AsIx ConwayEra)
      (Data ConwayEra)
      ExUnits

instance DecCBOR ConwaySingleRedeemer where
  decCBOR =
    decodeRecordNamed "Redeemer" (const 4) $
      ConwaySingleRedeemer <$> decCBORGroup <*> decCBOR <*> decCBOR

instance EncCBOR ConwaySingleRedeemer where
  encCBOR (ConwaySingleRedeemer purpose redeemerData exUnits) =
    encodeListLen 4
      <> encCBORGroup purpose
      <> encCBOR redeemerData
      <> encCBOR exUnits

-- | The ledger has no type for an integer on its own: it reads one inside plutus data.
--   The plutus data decoder refuses a bignum of more than 64 bytes, as @bounded_bytes@ states.
newtype PlutusInteger era = PlutusInteger (Data era)

instance (Typeable era) => DecCBOR (PlutusInteger era) where
  decCBOR = do
    plutusData <- decCBOR
    case getPlutusData plutusData of
      PV1.I _ -> pure (PlutusInteger plutusData)
      _ -> fail "expected an integer"

instance (Typeable era) => EncCBOR (PlutusInteger era) where
  encCBOR (PlutusInteger plutusData) = encCBOR plutusData

-- | The ledger has no type for bytes of limited size on their own: it reads them inside plutus data.
--   The plutus data decoder limits the size of a definite-length string to 64 bytes. It also accepts an
--   indefinite-length string of any size, if each chunk has at most 64 bytes.
newtype PlutusBytes era = PlutusBytes (Data era)

instance (Typeable era) => DecCBOR (PlutusBytes era) where
  decCBOR = do
    plutusData <- decCBOR
    case getPlutusData plutusData of
      PV1.B _ -> pure (PlutusBytes plutusData)
      _ -> fail "expected bytes"

instance (Typeable era) => EncCBOR (PlutusBytes era) where
  encCBOR (PlutusBytes plutusData) = encCBOR plutusData

-- | The rule for a multi-asset value whose amounts are positive.
--
--   The ledger decodes such a value only inside @value@, with a decoder that it does not export. The 'MultiAsset'
--   instance is the decoder of a mint, which accepts negative amounts and refuses the amounts above the 'Int64' range.
--   So the sample becomes the second item of the array @[0, sample]@, and 'MaryValue' decodes that array.
mkPositiveMultiAssetRule :: Version -> RuleName -> GenerationRule
mkPositiveMultiAssetRule version name = makeRule name decodeMultiAsset (serialize' version) normalized
  where
    decodeMultiAsset bytes = do
      MaryValue _ multiAsset <- decodeAs @MaryValue version (BS.pack [0x82, 0x00] <> bytes)
      pure multiAsset
