from collections import OrderedDict
from typing import Literal, TypedDict, Optional


class EStreamType:
    aggTrade = 'aggTrade'


class IPrice(TypedDict):
    symbol: str
    price: str  # float


class IFill(TypedDict):
    price: str  # float
    qty: str  # float
    commission: str  # float
    commissionAsset: str
    tradeId: int


class IOrder(TypedDict):
    symbol: str
    orderId: int
    orderListId: int
    clientOrderId: str
    transactTime: int
    price: str  # float
    origQty: str  # float
    executedQty: str  # float
    cummulativeQuoteQty: str  # float
    status: str
    timeInForce: str
    type: str
    side: str
    fills: list[IFill]


class ITokenStat(TypedDict):
    symbol: str
    token_price: float
    token_cost: float
    token_quantity: float
    sl_price: float
    last_order: Optional[IOrder]
    records: OrderedDict[int, float]


class IBalance(TypedDict):
    asset: str
    free: str  # float
    locked: str  # float


class IAccountInfo(TypedDict):
    makerCommission: int
    takerCommission: int
    buyerCommission: int
    sellerCommission: int
    canTrade: bool
    canWithdraw: bool
    canDeposit: bool
    updateTime: int
    accountType: str
    balances: list[IBalance]
    permissions: list[str]


class IAggTradeData(TypedDict):
    e: Literal['aggTrade']  # Event type
    E: int  # Event time
    s: str  # Symbol
    a: int  # Aggregate trade ID
    p: str  # Price (float)
    q: str  # Quantity (float)
    f: int  # First trade ID
    l: int  # Last trade ID
    T: int  # Trade time
    m: bool  # Is the buyer the market maker?
    M: bool  # Ignore


class IAggTrade(TypedDict):
    stream: str
    data: IAggTradeData
