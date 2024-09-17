package com.cosmetic.gg.entity.product;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Index;
import javax.persistence.Table;

import com.cosmetic.gg.entity.EntityBase;
import com.cosmetic.gg.entity.EntityCommon;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Table(name = "favorite", indexes = {
    @Index(name = "idx_favorite_product_id", columnList = "product_id"),
    @Index(name = "idx_favorite_user_id", columnList = "user_id")
})
@Entity
@Getter
@Setter
public class Favorite extends EntityCommon{

	@Column(name = "product_id")
	private String productId;
	
	@Column(name = "user_id")
	private String userId;
}
