package com.cosmetic.gg.entity.product;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Index;
import javax.persistence.Lob;
import javax.persistence.Table;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

import com.cosmetic.gg.entity.EntityCommon;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Table(name = "review", indexes = {
    @Index(name = "idx_review_user_id", columnList = "user_id"),
    @Index(name = "idx_review_product_item_id", columnList = "product_item_id"),
    @Index(name = "idx_review_comment", columnList = "comment"),
    @Index(name = "idx_review_star", columnList = "star")
})
@Entity
@Getter
@Setter
public class Review extends EntityCommon{

	@NotEmpty(message = "Id of user can not empty")
	@NotNull(message = "Id of user can not null")
	@Column(name = "user_id")
	private String userId;
	
	@NotEmpty(message = "Id of product not empty")
	@NotNull(message = "Id of product can not null")
	@Column(name = "product_item_id")
	private String productItemId;
	
	@Column(name = "comment")
	private String comment;
	
	@Column(name = "star")
	private Integer star;
}
