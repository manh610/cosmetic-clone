package com.cosmetic.gg.entity.order;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

import com.cosmetic.gg.entity.EntityBase;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Table(name = "order_item")
@Entity
@Getter
@Setter
public class OrderItem extends EntityBase{

	@NotEmpty(message = "Id of product item can not empty")
	@NotNull(message = "Id of product item can not null")
	@Column(name = "product_item_id")
	private String productItemId;
	
	@NotEmpty(message = "Id of order not empty")
	@NotNull(message = "Id of order can not null")
	@Column(name = "order_id")
	private String orderId;
	
	@Column(name = "quantity")
	private Integer quantity;
}
