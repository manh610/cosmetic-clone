package com.cosmetic.gg.entity.product;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Index;
import javax.persistence.Table;

import com.cosmetic.gg.entity.EntityCommon;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Table(name = "cart", indexes = {
    @Index(name = "idx_cart_product_item_id", columnList = "product_item_id"),
    @Index(name = "idx_cart_user_id", columnList = "user_id"),
    @Index(name = "idx_cart_quantity", columnList = "quantity")
})
@Entity
@Getter
@Setter
public class Cart extends EntityCommon{
	
	@Column(name = "quantity")
	private Integer quantity;

	@Column(name = "product_item_id")
	private String productItemId;
	
	@Column(name = "user_id")
	private String userId;
}
