package com.cosmetic.gg.entity.order;

import java.time.LocalDateTime;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Table;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

import com.cosmetic.gg.common.enums.EDeliveryType;
import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.entity.EntityBase;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Table(name = "order_product")
@Entity
@Getter
@Setter
public class OrderProduct extends EntityBase{	
	
	@Column(name = "address_id")
	private String addressId;
	
	@Column(name = "delivery_type")
	@Enumerated(EnumType.STRING)
	private EDeliveryType deliveryType;
	
	@Column(name = "payment_id")
	private String paymentId;
	
	@Column(name = "order_date")
	private LocalDateTime orderDate;
	
	@Column(name = "delivery_date")
	private LocalDateTime deliveryDate;
	
	@Column(name = "receipt_date")
	private LocalDateTime receiptDate;
	
	@Column(name = "censor")
	private String censor;
	
	@Column(name = "shipper")
	private String shipper;
	
	@Column(name = "is_payment")
	private boolean isPayment;
	
	@Column(name = "note")
	private String note;
	
	@Column(name = "status")
	@Enumerated(EnumType.STRING)
	private EStatus status;
	
	@Column(name = "total_price")
	private Float totalPrice;
	
	@Column(name = "delivery_unit_id")
	private String deliveryUnitId;
	
	@NotNull(message = "Id of user can not null")
	@NotEmpty(message = "Id of user can not empty")
	@Column(name = "user_id")
	private String userId;
	
	@Column(name = "discount_id")
	private String discountId;
}
